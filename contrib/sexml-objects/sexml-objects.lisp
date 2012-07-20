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
;;if the :set-as-html-id-p slot-attrib of the %id slot is set to t,
;;the value is set as the id attribute of the widget as well to be
;;rendered as the value for the id attribute of the widget
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
