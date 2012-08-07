;; sexml-objects.lisp

(in-package :sexml-objects)

(defclass widget (attributes-object)
  (;; system related
   (%id              :accessor %id              :initarg :%id              :initform nil)
   (%parent          :accessor %parent          :initarg :%parent          :initform nil)
   (%render-func     :accessor %render-func     :initarg :%render-func     :initform nil)
   (%children        :accessor %children        :initarg :%children        :initform nil)
   (%template-access :accessor %template-access :initarg :%template-access :initform nil))
  (:documentation "this is the base for widget classes")
  (:metaclass attributes-class))

;;;;generates automated-id for instances
(let ((instance-count-table (make-hash-table :test 'equal)))

  (defmethod initialize-instance :after ((obj widget) &rest initargs)
    (declare (ignore initargs))
    (if (atom (%children obj)) (setf (%children obj) (list (%children obj))))
    (update-children-parent obj)
    (if (gethash (type-of obj) instance-count-table)
	(incf (gethash (type-of obj) instance-count-table))
	(setf (gethash (type-of obj) instance-count-table) 1))
    (if (null (%id obj))
	(setf (%id obj) (format nil "~a-~a" (type-of obj) (gethash (type-of obj) instance-count-table)))
	(setf (%template-access obj) t))))

;;;;automatically set parents of the widget children to widget, children must always be in a list
(defmethod (setf %children) (new-value (widget widget))
  (setf new-value (if (listp new-value)
		      (remove nil new-value)
		      new-value))
  (if (null new-value)
      (setf (slot-value widget '%children) nil)
      (progn
	(if (atom new-value) (setf new-value (list new-value)))
	(setf (slot-value widget '%children) new-value)
	(update-children-parent widget)
	(slot-value widget '%children))))

(defun update-children-parent (widget)
  (mapcar (lambda (w)
	    (if (widgetp w)
		(progn
		  (setf (%parent w) widget)
		  (update-children-parent w))))
	  (%children widget)))

(defmethod %children ((obj t))
  (declare (ignore obj))
  nil)

(defmethod %changedp ((obj t))
  (declare (ignore obj))
  nil)

(defun map-widget (widget func)
  "maps the function over the children of the given widget and over the children of the child if it's a widget,
collecting any non nil values returned by func."
  (if (widgetp widget)
      (loop
	 for child in (%children widget)
	 for func-result = (funcall func child)
	 with result = nil
	 do (if func-result (push func-result result))
	 (if (widgetp widget)
	     (progn
	       (setf result (concatenate 'list result (map-widget child func)))))
	 finally (return result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric render-markup (widget)
  (:documentation "this method is called on the widgets that need to be rendered,
should return the html markup of the widget"))

(defmethod render-markup ((widget widget))
  (apply (%render-func widget)
	 `(,@(get-html-attributes widget)
	     ,@(if (car (%children widget))
		   (mapcar #'render-markup (%children widget))))))

(defmethod render-markup ((widget string))
  widget)

(defmethod render-markup ((widget function))
  (funcall widget))

(defmethod render-markup ((widget t))
  nil)

(defgeneric get-html-attributes (widget)
  (:documentation "finds all the slots in the given widget that have
the :html-attrip-p set to t and have a non nil value and returns a
plist that is used to render the attributes of the widget in the render-markup function.
the plist is made from the :keword attribute and slot-value of the slots."))

(defmethod get-html-attributes ((widget widget))
  (loop
     for slot-def in (closer-mop:class-slots (class-of widget))
     for slot-name = (closer-mop:slot-definition-name slot-def)
     for slot-value = (slot-value widget slot-name)
     with results = nil
     do (cond ((and (attributed-slot-p slot-def) (slot-attrib widget slot-name :html-attrib-p) slot-value)
	       (push slot-value results)
	       (push (slot-attrib widget slot-name :keyword) results)))
     finally (return results)))

;;return nil as the parent for string and function widgets
(defmethod %parent ((obj string)) nil)
(defmethod %parent ((obj function)) nil)

(defun widgetp (object)
  (typep object 'widget))

(defun add-child (widget child &optional (index 0) (from-end t))
  (let ((children (slot-value widget '%children)))
    (setf (%children widget)
	  (if from-end
	      (let ((index-from-end (- (length children) index)))
		(append (subseq children 0 index-from-end) (cons child (subseq children index-from-end))))
	      (append (subseq children 0 index) (cons child (subseq children index)))))))

(defun remove-child (widget &optional child (index 0) (from-end t))
  (let ((children (slot-value widget '%children)))
    (setf (%children widget)
	  (if child
	      (remove child children)
	      (if from-end
		  (let ((index-from-end (- (length children) index)))
		    (append (subseq children 0 (1- index-from-end)) (subseq children index-from-end)))
		  (append (subseq children 0 index) (subseq children (1+ index))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPER MACRO FOR EASY DEFINING OF UI;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-children (input)
  (loop
     for item in (cdr input)
     for item-counter = 0 then (1+ item-counter)
     with start-collecting = nil
     with results = nil
     do
       (if (null start-collecting)
	   (cond ((and (evenp item-counter) (null (keywordp item)))
		  (setf start-collecting t)
		  (push item results)))
	   (push item results))
     finally
       (setf results (reverse results))
       (return (if results
		   (concatenate 'list (subseq input 0 (position (car results) input)) `(:%children (list ,@results)))
		   input))))

(defun expand-to-sexp (input &optional (package '<))
  (if input
      (if (listp input)
	  (if (listp (car input))
	      `(,@(cons (expand-to-sexp (car input)) (mapcar #'expand-to-sexp (cdr input))))
	      (let ((class nil))
		(setf class (or (ignore-errors (find-class (car input)))
				(ignore-errors (find-class (intern (string (car input)) (find-package package))))))
		(cond (class
		       (if (null (closer-mop:class-finalized-p class)) (closer-mop:finalize-inheritance class))
		       (if (find (find-class 'widget) (closer-mop:class-precedence-list class))
			   (progn
			     (setf input (setup-children input))
			     `(make-instance ',(class-name class) ,@(mapcar #'expand-to-sexp (cdr input))))
			   (cons (car input) (mapcar #'expand-to-sexp (cdr input)))))
		      (t (cons (car input) (mapcar #'expand-to-sexp (cdr input)))))))
	  input)
      'nil))

(defmacro define-ui ((&optional (package '<)) &body body)
  `(progn ,@(mapcar (lambda (part) (expand-to-sexp part package)) body)))

(defparameter *templates* (make-hash-table))

(defmacro define-template ((&optional name (package '<)) &body body)
  (if name
      `(setf (gethash ',name *templates*) (lambda () (define-ui (,package) ,@body)))
      `(lambda () (define-ui (,package) (,@body)))))

(defun get-template (name)
  (gethash name *templates*))

(defmacro define-template-from-file ((template-name file-spec &optional (package '<)))
  (let ((file-spec (eval file-spec)))
    `(progn
       (setf (gethash ',template-name *templates*)
	     (lambda () (define-ui (,package) ,(read-from-string (convert-html-string (read-template-file file-spec)) nil))))
       (funcall (get-template ',template-name)))))

(defun read-template-file (file-spec)
  (let ((input-string ""))
    (with-open-file (file-stream file-spec)
      (loop for line = (read-line file-stream nil 'foo)
	 until (eq line 'foo)
	 do (setf input-string (concatenate 'string input-string line (string #\newline)))))
    input-string))

(defun convert-html-string (string)
  (let ((result ""))
    (with-input-from-string (stream string)
      (loop for c = (read-char stream nil #\null)
	 do (cond ((and (char= c #\<) (char= (peek-char nil stream) #\!))
		   (loop for c = (read-char stream)
		      until (char= c #\>)))
		  ((and (char= c #\<) (char= (peek-char nil stream) #\#))
		   (setf result (concatenate 'string result (read-lisp-form stream))))
		  ((char= c #\<)
		   (setf result (concatenate 'string result (read-tag stream) " ")))
		  ((char= c #\null) (return-from convert-html-string result))
		  (t
		   (cond ((not (char= (peek-char t stream nil #\null) #\<))
			  (unread-char c stream)
			  (setf result (concatenate 'string result "\"" (read-tag-content stream) "\"")))
			 (t
			  (unread-char c stream)
			  (cond ((not (char= (peek-char t stream nil #\null) #\<))
				 (setf result (concatenate 'string result "\"" (read-tag-content stream) "\""))))))))))))

(defun read-tag (stream)
  (let ((result "("))
    (if (char= (peek-char nil stream) #\/)
	(progn
	  (loop for c = (read-char stream)
	     while (not (char= c #\>)))
	  (return-from read-tag ")"))
	(progn
	  (loop for c = (read-char stream nil #\null)
	     do (cond ((and (char= c #\<) (char= (peek-char nil stream) #\#))
			(setf result (concatenate 'string result " " (read-lisp-form stream) " ")))
		      ((and (char= c #\space) (not (char= (peek-char t stream) #\/)) (not (char= (peek-char t stream) #\>)) (not (char= (peek-char t stream) #\<)))
		       (setf result (concatenate 'string result (read-attrib stream))))
		      ((char= c #\/)
		       (read-char stream)
		       (setf result (concatenate 'string result " )"))
		       (return-from read-tag result))
		      ((char= c #\>)
		       (return-from read-tag result))
		      ((char= c #\null)
		       (return-from read-tag result))
		      (t 
		       (setf result (concatenate 'string result (string c))))))))))

(defun read-attrib (stream)
  (let ((attrib-name "")
	(result ""))
    (loop for c = (read-char stream nil #\null)
       do (setf attrib-name (concatenate 'string attrib-name (string c)))
       do (if (or (char= (peek-char t stream nil #\null) #\/) (char= (peek-char t stream nil #\null) #\>) (char= (peek-char t stream nil #\null) #\>))
	      (return-from read-attrib (concatenate 'string "\"" attrib-name "\"") ))
       until (char= (peek-char t stream nil #\null) #\=))
    (read-char stream)
    (setf result (concatenate 'string " :" attrib-name " " (read-string stream)))
    result))

(defun read-string (stream)
  (let ((result "\""))
    (read-char stream)
    (loop for c = (read-char stream nil #\null)
       do (setf result (concatenate 'string result (string c)))
       until (char= c #\"))
    result))

(defun read-lisp-form (stream)
  (let ((result " "))
    (read-char stream)
    (loop for c = (read-char stream nil #\null)
       until (and (char= c #\#) (char= (peek-char nil stream) #\>))
       do (setf result (concatenate 'string result (string c))))
    (read-char stream)

    result))

(defun read-tag-content (stream)
  (let ((result ""))
    (loop for c = (read-char stream nil #\null)
       do (setf result (concatenate 'string result (string c)))
       until (or (char= (peek-char nil stream nil #\null) #\<)
		 (char= (peek-char nil stream nil #\null) #\null)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UTILITY MACRO FOR AUTO-ACCESS OF TEMPLATE WIDGETS;;;;;;;;;;;;;;;;;;;
(defun widget-by-id (id root-widget)
  (if root-widget
      (car (map-widget root-widget
		       (lambda (widget)
			 (if (and (widgetp widget) (string= (%id widget) id))
			     widget
			     nil))))))

(defmacro with-template ((template-name) &body body)
  (let* ((template (funcall (get-template template-name)))
	 (temp-symb (intern (string-upcase (concatenate 'string "=" (symbol-name template-name) "="))))
	 (widget-list (map-widget template (lambda (w)
					     (if (and (widgetp w) (%template-access w))
						 `(,(intern (string-upcase (%id w))) (widget-by-id ,(%id w) ,temp-symb))
						 nil)))))
    `(let* ((,temp-symb (funcall (get-template ',template-name)))
	    ,@widget-list)
       (declare (ignorable ,@(mapcar #'car widget-list)))
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEFINE LAYERED METHOD AND RELATED SEXML CODES;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-sexml-objects (dtd-path target-package inherit-list)
  (eval
     (read-from-string
      (concatenate
       'string
       "(progn
     (contextl:deflayer widget-classes)
     (contextl:define-layered-method sexml::entity-definition-forms :in-layer widget-classes :around (entity package)
				     (let ((class-symbol (sexml::function-symbol entity package)))
				       `((defclass ,class-symbol (sexml-objects:widget "
       (format nil "~{ ~a ~}" inherit-list)
       ")
					   ((sexml-objects:%render-func :accessor sexml-objects:%render-func 
									:initarg :%render-func 
									:initform #',class-symbol)
					    ,@(loop for attribute-string in (sexml::attributes entity)
						 for attribute-symbol = (sexml::argument-symbol attribute-string package)
						 for attribute-key = (sexml::argument-symbol attribute-string :keyword)
do 
(export `(,(intern (concatenate 'string \"GET-\" (format nil \"~A\" attribute-symbol)))
	  ,(intern (concatenate 'string \"SET-\" (format nil \"~A\" attribute-symbol)))))

						 collect `(,attribute-symbol :reader ,(intern (concatenate 'string \"GET-\" (format nil \"~A\" attribute-symbol)))
									     :writer ,(intern (concatenate 'string \"SET-\" (format nil \"~A\" attribute-symbol)))
									     :initarg ,attribute-key :initform nil
									     :attributes (:html-attrib-p t :keyword ,attribute-key))))
					   (:metaclass cl-attribs:attributes-class))
					 ,@(call-next-method))))

     (sexml:with-compiletime-active-layers (sexml:standard-sexml sexml-objects:widget-classes)
       (sexml:support-dtd "
       (format nil "\"~a\" :~a" dtd-path target-package)
       ")))"))))
