;; sexml-objects.lisp

(in-package :sexml-objects)

(defclass widget (attributes-object)
  (;; system related
   (%id          :accessor %id          :initarg :%id          :initform nil)
   (%parent      :accessor %parent      :initarg :%parent      :initform nil)
   (%render-func :accessor %render-func :initarg :%render-func :initform nil)
   (%children    :accessor %children    :initarg :%children    :initform nil))
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
	(setf (%id obj) (format nil "~a-~a" (type-of obj) (gethash (type-of obj) instance-count-table))))))

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

(defun expand-to-sexp (input)
  (if input
      (if (listp input)
	  (if (listp (car input))
	      `(,@(cons (expand-to-sexp (car input)) (mapcar #'expand-to-sexp (cdr input))))
	      (let ((class nil))
		(ignore-errors (setf class (find-class (car input))))
		(cond (class
		       (if (null (closer-mop:class-finalized-p class)) (closer-mop:finalize-inheritance class))
		       (if (find (find-class 'widget) (closer-mop:class-precedence-list class))
			   (progn
			     (setf input (setup-children input))
			     `(make-instance ',(car input) ,@(mapcar #'expand-to-sexp (cdr input))))
			   (cons (car input) (mapcar #'expand-to-sexp (cdr input)))))
		      (t (cons (car input) (mapcar #'expand-to-sexp (cdr input)))))))
	  input)
      'nil))

(defmacro define-ui (&body body)
  `(progn ,@(mapcar #'expand-to-sexp body)))


(defparameter *templates* (make-hash-table))

(defmacro define-template ((&optional name) &body body)
  (if name
      `(setf (gethash ',name *templates*) (lambda () (define-ui ,@body)))
      `(lambda () (define-ui (,@body)))))

(defun get-template (name)
  (gethash name *templates*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DEFINE LAYERED METHOD AND RELATED SEXML CODES;;;;;;;;;;;;;;;;;;;;;;;
(contextl:deflayer widget-classes)

(contextl:define-layered-method sexml::entity-definition-forms
  :in-layer widget-classes
  :around (entity package)
  (let ((class-symbol (sexml::function-symbol entity package)))

;;define a class for each tag, they should inherit from widget feel free to add 
;;your own classes for more functionality. some usefull classes are included in
;;this system. check cl-binds and cl-changes.

    `((defclass ,class-symbol (sexml-objects:widget)

;;%render-func is the function used to generate the markup for the class, the 
;;default function is the one created with sexml

	((sexml-objects:%render-func :accessor sexml-objects:%render-func 
				     :initarg :%render-func 
				     :initform #',class-symbol)
	 ,@(loop for attribute-string in (sexml::attributes entity)
	      for attribute-symbol = (sexml::argument-symbol attribute-string 
	      	  		     			     package)
	      for attribute-key = (sexml::argument-symbol attribute-string 
	      	  		  			  :keyword)

;;define reader and writer methods of each attribute. I use GET-* and SET-* 
;;methods to make it clear that they are sexml generated attribute methods

	      collect `(,attribute-symbol :reader ,(intern (concatenate 'string 
	      	      			  "GET-" (format nil "~A" 
					  	 attribute-symbol)))
					  :writer ,(intern (concatenate 'string 
					  "SET-" (format nil "~A" 
					  	 attribute-symbol)))
					  :initarg ,attribute-key :initform nil

;;this is some functionality provided by the cl-attribs that allows us to 
;;define attributes for the slots of a class, these attribs are used by
;;sexml-objects to render the markup for the class.
;;the :html-attrib-p tells sexml-objects that this slot contains a attribute
;;that should be inserted in the markup. :keyword is the keyword that will be
;;sent to the %render-func along with the value of this slot. 

					  :attributes (:html-attrib-p t :keyword 
					  	      ,attribute-key))))

;;the attributes-class is the metaclass for every object that sexml-objects
;;creates

	(:metaclass cl-attribs:attributes-class))
      ,@(call-next-method))))
