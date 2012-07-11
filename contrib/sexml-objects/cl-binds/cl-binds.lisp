;;;; cl-binds.lisp

(in-package #:cl-binds)

(defclass bindable-object (attributes-object)
  ((%bound-object :accessor %bound-object :initarg :%bound-object :initform nil))
  (:metaclass attributes-class)
  (:documentation "if %bound-object is non nil, every slot that provides
a :bound-slot-name will read/write it's value from/to the slot of %bound-object"))

(defmethod closer-mop:slot-value-using-class :around ((class attributes-class) (object bindable-object) (slotd attributed-effective-slot))
  (let* ((slot-name (closer-mop:slot-definition-name slotd))
	 (bound-slot-name (slot-attrib object slot-name :bound-slot-name)))
    (if (and bound-slot-name (%bound-object object))
	(slot-value (%bound-object object) bound-slot-name)
	(call-next-method))))

(defmethod (setf closer-mop:slot-value-using-class) :around (new-value (class attributes-class) (object bindable-object) (slotd attributed-effective-slot))
  (cond ((slot-boundp object '%all-attributes)
	 (let* ((slot-name (closer-mop:slot-definition-name slotd))
		(bound-slot-name (slot-attrib object slot-name :bound-slot-name)))
	   (if (and bound-slot-name (%bound-object object))
	       (setf (slot-value (%bound-object object) bound-slot-name) new-value)
	       (call-next-method))))
	(t
	 (call-next-method)))
  (call-next-method))

;;this is just to make sure there is no binding during the init phase, to prevent bound object from getting messed up!
(defmethod initialize-instance :around ((object bindable-object) &rest initargs)
  (let ((bound-object (getf initargs :%bound-object)))
    (setf (getf initargs :%bound-object) nil)
    (let ((result (call-next-method)))
      (setf (%bound-object result) bound-object)
      result)))
