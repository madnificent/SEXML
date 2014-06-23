;;;; cl-binds.lisp

(in-package #:cl-binds)

(defclass bindable-object (attributes-object)
  ((%bound-object :accessor %bound-object :initarg :%bound-object :initform nil))
  (:metaclass attributes-class)
  (:documentation "if %bound-object is non nil, every slot that provides
a :bound-slot-name will read/write it's value from/to the slot of %bound-object"))

(defmethod closer-mop:slot-value-using-class :around ((class attributes-class) (object bindable-object) (slotd attributed-effective-slot))
  (let* ((slot-name (closer-mop:slot-definition-name slotd))
	 (bound-slot (slot-attrib object slot-name :bound-slot)))
    (cond ((and bound-slot (%bound-object object))
	   (cond ((listp bound-slot)
		  (let ((reader-func (or (getf bound-slot :reader) #'identity))
			(bound-slot-name (getf bound-slot :name)))
		    (cond (reader-func
			   (funcall reader-func (slot-value (%bound-object object) bound-slot-name)))
			  (t 
			   (slot-value (%bound-object object) bound-slot-name)))))
		 (t
		  (slot-value (%bound-object object) bound-slot))))
	  (t 
	   (call-next-method)))))

(defmethod (setf closer-mop:slot-value-using-class) :around (new-value (class attributes-class) (object bindable-object) (slotd attributed-effective-slot))
  (cond ((slot-boundp object '%all-attributes)
	 (let* ((slot-name (closer-mop:slot-definition-name slotd))
		(bound-slot (slot-attrib object slot-name :bound-slot)))
	   (cond ((and bound-slot (%bound-object object))
		  (cond ((listp bound-slot)
			 (let ((writer-func (or (getf bound-slot :writer) #'identity))
			       (bound-slot-name (getf bound-slot :name)))
			   (setf (slot-value (%bound-object object) bound-slot-name) (funcall writer-func new-value))))
			(t
			 (setf (slot-value (%bound-object object) bound-slot) new-value))))))))
  (call-next-method))

;;this is just to make sure there is no binding during the init phase, to prevent bound object from getting messed up!
(defmethod initialize-instance :around ((object bindable-object) &rest initargs)
  (let ((bound-object (getf initargs :%bound-object))
	(new-args (concatenate 'list `(:%bound-object nil) initargs)))
    (let ((result (apply #'call-next-method `(,object ,@new-args))))
      (setf (%bound-object result) bound-object)
      result)))
