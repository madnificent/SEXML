
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

(define-layered-function dtd-support-forms (dtd package)
  (:documentation "returns a list of forms which need to be compiled to support the dtd")
  (:method (dtd package)
    nil))

(define-layered-function entity-printer-forms (entity attr-var body)
  (:documentation "produces the forms which will handle the printing of the tags.  <entity> contains the entity which needs to be printed.  <attr-var> contains a symbol which will contain a plist of attribute-value pairs, the keyword must constist of a string at runtime, the value is not specified.  <body> contains a symbol which will contain a list of content which must be printed within the tag."))

(define-layered-function tag-attribute-content (content)
  (:documentation "prints <content> in a way that it's a valid value for an attribute")
  (:method (content)
    (typecase content
      (string
       (cl-ppcre:regex-replace "\"" content "&quot;"))
      (list
       (tag-attribute-content (format nil "~{~A~^ ~}" content)))
      (T (tag-attribute-content (format nil "~A" content))))))

(defun recursively-flatten (&rest args)
  "recursively flattens a list"
  (loop for arg in args
     append (if (listp arg)
                (apply #'recursively-flatten arg)
                (list arg))))

(define-layered-function tag-body-content (content)
  (:documentation "prints <content> in a way appropriate for xml output.  output functions should use this in order to create correct output.")
  (:method (content)
    (format nil "~{~A~}" (recursively-flatten content))))

(defmacro support-dtd (file packagename)
  (let ((dtd (mk-dtd-object (eval file)))
        (package (mk-package-object packagename)))
    `(progn ,@(dtd-support-forms dtd package)
        ,@(loop for element in (dtd-elements dtd)
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
(deflayer sexml-xml-producer ())

(defun format-tag-attr-content (stream arg colonp atp &rest options)
  (declare (ignore colonp atp options))
  (format stream "~A" (tag-attribute-content arg)))

(defun format-tag-body-content (stream arg colonp atp &rest options)
  (declare (ignore colonp atp options))
  (format stream "~A" (tag-body-content arg)))

(define-layered-method entity-printer-forms
  :in-layer sexml-xml-producer
  (entity attr-var body)
  `(format nil ,(concatenate 'string
                             "<" (name entity) "~{ ~A=\"~/sexml::format-tag-attr-content/\"~}" (if (subelements-p entity) ">" "/>") ;; tag
                             (when (subelements-p entity)
                               "~{~/sexml::format-tag-body-content/~}") ;; content
                             (when (subelements-p entity)
                               (concatenate 'string "</" (name entity) ">")))
           ,@(if (null (subelements-p entity))
                 (list attr-var)
                 (list attr-var body))))

  
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
                                   append (list (getf key-translations (pop args)) ;; we pop args, so args contains the body in the end
                                                (pop args)))))))
            ,(entity-printer-forms entity 'keys 'args))))
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

(deflayer xml-comments ())

(define-layered-method dtd-support-forms
  :in-layer xml-comments
  :around (dtd package)
  (let ((function-name (mk-lisp-symbol (symbol-name '!--) package)))
    `(,@(call-next-method)
        (defun ,function-name (&rest comments)
          (format nil "<!-- ~{~A~} -->" (recursively-flatten comments)))
        (export (quote ,function-name) (symbol-package (quote ,function-name))))))

(deflayer ie-conditionals ())

(define-layered-method dtd-support-forms
  :in-layer ie-conditionals
  :around (dtd package)
  (let ((function-name (mk-lisp-symbol (symbol-name '!if) package)))
    `(,@(call-next-method)
      (defun ,function-name (condition &rest args)
          (format nil "<!--[if ~A]>~{~A~}<![endif]-->" condition (recursively-flatten args)))
      (export (quote ,function-name)
              (symbol-package (quote ,function-name))))))

(deflayer xml-doctype ())

(define-layered-method dtd-support-forms
  :in-layer xml-doctype
  :around (dtd package)
  (let ((doctype-var (mk-lisp-symbol (symbol-name '*doctype*) package))
        (doctype-func (mk-lisp-symbol (symbol-name 'doctype) package))
        (doctype-add-dtd (mk-lisp-symbol (symbol-name 'augment-with-doctype) package))
        (doctype-add-func (mk-lisp-symbol (symbol-name 'augment-tag-with-doctype) package)))
    `(,@(call-next-method)
        (defparameter ,doctype-var "" "Set this to the doctype for this xml package")
        (defun ,doctype-func (&rest content)
          (format nil "~A~&~{~A~}" ,doctype-var (recursively-flatten content)))
        (defmacro ,doctype-add-func (function-symbol)
          (list 'setf (list 'fdefinition (list 'quote function-symbol))
                (list 'let (list (list 'function (list 'function function-symbol)))
                      '(lambda (&rest args)
                        (concatenate 'string 
                         (funcall ',doctype-func)
                         (apply function args))))))
        (defmacro ,doctype-add-dtd (tag dtd &key auto-emit-p)
          (list 'progn
                (list 'setf ',doctype-var (list
                                           'format 'nil
                                           "<!DOCTYPE ~A ~A>"
                                           tag dtd))
                (when auto-emit-p
                  (list ',doctype-add-func
                        (mk-lisp-symbol tag (symbol-package (quote ,doctype-add-dtd)))))))
        (export (quote ,doctype-var)
                (symbol-package (quote ,doctype-var)))
        (export (quote ,doctype-func)
                (symbol-package (quote ,doctype-func)))
        (export (quote ,doctype-add-func)
                (symbol-package (quote ,doctype-add-func)))
        (export (quote ,doctype-add-dtd)
                (symbol-package (quote ,doctype-add-dtd))))))

(deflayer standard-sexml (export-function-symbol
                          #+swank swank-sexml-documented-attributes
                          sexml-functions
                          sexml-xml-producer
                          xml-comments))
