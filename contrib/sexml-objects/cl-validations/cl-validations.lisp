;;;; cl-validations.lisp

(in-package #:cl-validations)

(defun object-validp (object)
  (if (typep object 'attributes-object)
      (let ((slots (closer-mop:class-slots (class-of object)))
	    (results nil))
	(loop for slot in slots
	   do (let ((slot-name (closer-mop:slot-definition-name slot)))
		(multiple-value-bind (slot-result error-msg) (slot-validp object slot-name)
		  (if (null slot-result)
		      (push (list slot-name error-msg) results)))))
	(if results
	    (values nil results)
	    (values t nil)))
      (values t nil)))

(defun slot-validp (object slot-name)
  (if (typep object 'attributes-object)
      (let* ((validation (slot-attrib object slot-name :validation))
	     (validator-func (let ((validator (getf validation :test)))
			       (if validator
				   (if (stringp validator)
				       (lambda (string) (scan validator string))
				       validator)
				   nil)))
	     (error-msg (getf validation :error-msg ""))
	     (value (slot-value object slot-name))
	     (result (if validator-func
			 (funcall validator-func value)
			 t)))
	(if result
	    (values t nil)
	    (values nil error-msg)))
      (values t nil)))
