
(in-package :sexml)

(defun mk-package-object (name)
  "creates a new package object"
  (list (or (find-package name)
           (make-package name))))

(defun package-exports-symbol (package symbol)
  "makes sure package knows it needs to export symbol, and exports it"
  (export symbol (first package))
  (setf (cdr (last package)) (cons symbol nil))
  symbol)

(defun package-declaration (package)
  "creates a definition for the package"
  (let ((package (first package))
        (exports (rest package)))
    `(defpackage ,(package-name package)
       (:export ,@exports))))

(defun mk-lisp-symbol (entity package)
  (when (listp package)
    (setf package (first package)))
  (when (packagep package)
    (setf package (package-name package)))
  (setf entity (cl-ppcre:regex-replace-all "\\(" entity "<"))
  (setf entity (cl-ppcre:regex-replace-all "\\)" entity ">"))
  (setf entity (cl-ppcre:regex-replace-all " " entity "-"))
  (setf entity (cl-ppcre:regex-replace-all "_" entity "-"))
  (setf entity (cl-ppcre:regex-replace-all ":" entity "."))
  (setf entity (cl-ppcre:regex-replace-all "([a-z])([A-Z])" entity "\\1-\\2"))
  (setf entity (string-upcase entity)) ;; this is portable, but doesn't work nice on modern-mode i assume
  (intern entity (find-package package)))


(defclass dtd ()
  ((path :initarg :path :reader dtd-path)
   (elements-hash :initform (make-hash-table :test 'equal) :accessor dtd-elements-hash))
  (:documentation "Datastructure which contains all information of a DTD."))

(defclass element ()
  ((name :initarg :name :reader name)
   (attributes :initform nil :accessor attributes)
   (subelements-p :initarg :subelements-p :initform nil :accessor subelements-p)))

(defclass attribute ()
  ((name :initarg :name :reader name))
  (:documentation "represents a possible attribute for an element"))

(defgeneric function-symbol (element package)
  (:documentation "returns a symbol for the function of element in package")
  (:method ((element element) package)
    (mk-lisp-symbol (name element) package)))

(defgeneric argument-symbol (attribute package)
  (:documentation "returns a symbol for the argument which can be given to the attribute, imported in package")
  (:method ((attribute attribute) package)
    (mk-lisp-symbol (name attribute) package)))

(defgeneric dtd-elements (dtd)
  (:documentation "returns the elements of the document")
  (:method (dtd)
    (loop for val being the hash-values of (dtd-elements-hash dtd)
       collect val)))

(defgeneric add-element (dtd element)
  (:documentation "adds <element> to the dtd>")
  (:method ((dtd dtd) (element element))
    (setf (gethash (name element) (dtd-elements-hash dtd))
          element)))

(defgeneric find-element (dtd name-string)
  (:documentation "searches for the element representing <name-string> in the dtd")
  (:method ((dtd dtd) name-string)
    (gethash name-string (dtd-elements-hash dtd))))

(defgeneric add-attribute (element attribute)
  (:documentation "registers the existence of <attribute> for <element>.")
  (:method ((element element) (attribute attribute))
    (push attribute (attributes element))))

(defun mk-dtd-object (file)
  (make-instance 'dtd :path file))

(defclass dtd-sax-handler (sax:default-handler)
  ((dtd :initarg :dtd :reader dtd))
  (:documentation "sax handler which calls the correct methods on its DTD"))

(defmethod sax:element-declaration ((handler dtd-sax-handler) name model)
  (add-element (dtd handler)
               (make-instance 'element
                              :name name
                              :subelements-p (not (eq model :empty)))))

(defmethod sax:attribute-declaration ((handler dtd-sax-handler) element-name attribute-name type default)
  (declare (ignore type default))
  (add-attribute (find-element (dtd handler) element-name)
                 (make-instance 'attribute :name attribute-name)))


(defmethod initialize-instance :after ((dtd dtd) &key path &allow-other-keys)
  (let ((handler (make-instance 'dtd-sax-handler :dtd dtd)))
    (cxml:parse-dtd-file path handler)))


(define-layered-function entity-definition-forms (entity package)
  (:documentation "entity-definition-forms is called with an entity and package object (both defined in sexml).  it should return all forms needed to generate the functions.")
  (:method (entity package)
    (declare (ignore entity package))
    nil))

(defmacro support-dtd (file packagename)
  (let ((dtd (mk-dtd-object file))
        (package (mk-package-object packagename)))
    `(progn ,@(loop for element in (dtd-elements dtd)
            collect `(progn ,@(entity-definition-forms element package))))))


(defmacro with-compiletime-active-layers ((&rest layers) &body body)
  (let ((layers-to-activate (loop for layer in layers
                               unless (contextl:layer-active-p layer)
                               collect layer)))
    (mapcar #'contextl:ensure-active-layer layers-to-activate)
    (dolist (bd body)
      (eval bd))
    (mapcar #'contextl:ensure-inactive-layer layers-to-activate)
    nil))
(deflayer sexml-functions ())
  
(define-layered-method entity-definition-forms
  :in-layer sexml-functions
  :around (entity package)
  (let ((sexp-entity (function-symbol entity package))
        (sexp-attributes (mapcar (rcurry #'argument-symbol :keyword)
                                 (attributes entity))))
    `((let* ((key-translations ',(loop for key in sexp-attributes
                                    for expansion in (attributes entity)
                                    append (list key (name expansion)))))
        (defun ,sexp-entity (&rest args)
          (let* ((keys ,(if (null (subelements-p entity))
                            `(loop for (a b) on args by #'cddr
                                append (list (getf key-translations a) b))
                            `(progn (loop while (keywordp (first args))
                                   append (list (getf key-translations (pop args))
                                                (pop args)))))))
            (format nil ,(concatenate 'string
                                      "<" (name entity) "~{ ~A=~S~}" (if (subelements-p entity) ">" "/>") ;; tag
                                      (when (subelements-p entity)
                                        "~{~A~}") ;; content
                                      (when (subelements-p entity)
                                        (concatenate 'string "</" (name entity) ">")))
                    ,@(if (null (subelements-p entity))
                          (list 'keys)
                          (list 'keys 'args))))))
      ,@(call-next-method))))
  
(deflayer export-function-symbol ())

(define-layered-method entity-definition-forms
  :in-layer export-function-symbol
  :around (entity package)
  (let ((symbol (function-symbol entity package)))
    `((export (quote ,symbol) (symbol-package (quote ,symbol)))
      ,@(call-next-method))))

(deflayer swank-sexml-documented-attributes ())

(define-layered-method entity-definition-forms
  :in-layer swank-sexml-documented-attributes
  :around (entity package)
  (let* ((symbol (function-symbol entity package))
         (attribute-symbols (mapcar (rcurry #'argument-symbol (car package))
                                    (attributes entity)))
         (attribute-keywords (mapcar (rcurry #'argument-symbol :keyword)
                                     (attributes entity))))
    `((defmethod swank:arglist-dispatch :around ((symbol (eql ',symbol)) arglist)
        (let ((arglist (call-next-method)))
          (setf (swank::arglist.keyword-args arglist)
                (loop for attr-sym in '(,@attribute-symbols)
                   for attr-key in '(,@attribute-keywords)
                   collect (swank::make-keyword-arg attr-key attr-sym nil)))
          (setf (swank::arglist.rest arglist)
                'rest)
          (setf (swank::arglist.key-p arglist) t)
          arglist))
      ,@(call-next-method))))

(deflayer standard-sexml (export-function-symbol
                          #+swank swank-sexml-documented-attributes
                          sexml-functions))
