;;;; cl-changes.lisp

(in-package #:cl-changes)

(defclass change-sensitive-object (attributes-object)
  ((%changedp  :initform t   :initarg :%changedp  :accessor %changedp)
   (%on-change :initform nil :initarg :%on-change :accessor %on-change
	       :documentation "a function to call when there is a change, format: (lambda (object slot-name))"))
  (:metaclass attributes-class)
  (:documentation "detects changes of the slot values and sets its changedp slot to t
and calls the function in the on-change slot, if the changed slot has an attribute of :on-change, that function will be called too.
the format for the slot's on change function is (lambda (object))"))

(defmethod (setf closer-mop:slot-value-using-class) :after (new-value class (object change-sensitive-object) slotd)
  (declare (ignorable new-value class slotd))
  (let ((slot-name (closer-mop:slot-definition-name slotd)))
    (if (slot-boundp object '%changedp)
	(cond ((not (or (eq slot-name '%changedp)
			(eq slot-name '%on-change)))
	       (setf (%changedp object) t)
	       (if (and (slot-boundp object '%on-change) (%on-change object))
		   (funcall (%on-change object) object slot-name))
	       (if (slot-attrib object slot-name :on-change)
		   (funcall (slot-attrib object slot-name :on-change) object)))))))

