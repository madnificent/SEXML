;;;; cl-attribs.lisp

(in-package #:cl-attribs)

(defun pairup-list (list)
  "returns a list containing lists with length of 2, made from the original list"
  (loop
     with results = nil
     with counter = 0
     while (<= counter (- (length list) 2))
     do
       (push (list (nth counter list) (nth (1+ counter) list)) results)
       (setf counter (+ counter 2))
     finally (return (reverse results))))

(defclass attributed-direct-slot (closer-mop:standard-direct-slot-definition)
  ((attributes :accessor attributes :initarg :attributes :initform nil)))

(defclass attributed-effective-slot (closer-mop:standard-effective-slot-definition)
  ((attributes :accessor attributes :initarg :attributes :initform nil)))

(defclass attributes-class (standard-class)
  ()
  (:documentation "This is the metaclass used for managing attributes-object"))

(defmethod closer-mop:validate-superclass ((c attributes-class) (sc standard-class)) t)

(defmethod closer-mop:direct-slot-definition-class ((class attributes-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'attributed-direct-slot))

(defun attributed-slot-p (slot)
  (or (typep slot 'attributed-direct-slot)
      (typep slot 'attributed-effective-slot)))

(defmethod closer-mop:compute-effective-slot-definition ((class attributes-class) name direct-slots)
  (if (every #'attributed-slot-p direct-slots)
      (let ((normal-slot (call-next-method))
	    (all-attributes (compute-attribute-inheritance direct-slots)))
	(setf all-attributes (append (mapcar #'eval all-attributes)))
	(make-instance 'attributed-effective-slot
		       :attributes (copy-tree all-attributes)
		       :allocation-class class
		       :allocation (closer-mop:slot-definition-allocation normal-slot)
		       :class class
		       :documentation (documentation normal-slot t)
		       :initargs (closer-mop:slot-definition-initargs normal-slot)
		       :writers (closer-mop:slot-definition-writers normal-slot)
		       :readers (closer-mop:slot-definition-readers normal-slot)
		       :initfunction (closer-mop:slot-definition-initfunction normal-slot)
		       :initform (closer-mop:slot-definition-initform normal-slot)
		       :name name))
      (call-next-method)))

(defun compute-attribute-inheritance (direct-slots)
  "removes duplicated attributes from the list with the latest value for each attribute"
  (let* ((all-attribs (reduce #'append (reverse direct-slots) :key 'attributes))
	 (filtered-pairs nil)
	 (attrib-pairs (pairup-list all-attribs)))
    (loop for item in attrib-pairs
       do (if (find (car item) filtered-pairs :key #'car)
	      (setf (cadr (assoc (car item) filtered-pairs)) (cadr item))
	      (push item filtered-pairs))
       finally (return (reduce #'append filtered-pairs)))))

(defmethod closer-mop:compute-slots ((class attributes-class))
  (let* ((slots (call-next-method))
	 (attributes (loop for slot in slots
			if (attributed-slot-p slot)
			collect (cons (closer-mop:slot-definition-name slot) (list (attributes slot))))))
    (cons (make-instance 'closer-mop:standard-effective-slot-definition
			 :name '%all-attributes
			 :initform attributes
			 :initfunction (lambda () attributes))
	  slots)))

(defclass attributes-object ()
  ()
  (:metaclass attributes-class)
  (:documentation "This is the class that provides functionality for getting and setting attributes of its slots.
use this format to set slot-value and slot-attribs at once:
(setf (some-slot object) '(:value/attribs (value (:attrib-a attrib-a-value))))"))

(defgeneric slot-attrib (object slot-name attrib-keyword)
  (:documentation "Returns the attribute value of the given slot of the given object"))
(defgeneric slot-attribs (object slot-name)
  (:documentation "Returns a list of all attributes and values of the given slot from the object"))

(defmethod slot-attrib ((object attributes-object) (slot-name symbol) (attrib-keyword symbol))
  (getf (cadr (assoc slot-name (slot-value object '%all-attributes))) attrib-keyword))

(defmethod slot-attribs ((object attributes-object) (slot-name symbol))
  (cadr (assoc slot-name (slot-value object '%all-attributes))))

(defun find-slot-initarg-by-name (object slot-name)
  (let ((all-slots (closer-mop:class-slots (class-of object))))
    (dolist (slot-def all-slots)
      (if (eq (closer-mop:slot-definition-name slot-def) slot-name)
	  (return-from find-slot-initarg-by-name (car (closer-mop:slot-definition-initargs slot-def)))))))

(defgeneric (setf slot-attrib) (new-value object slot-name attrib-name))
(defgeneric (setf slot-attribs) (new-value object slot-name))

(defmethod (setf slot-attrib) (new-value (object attributes-object) (slot-name symbol) (attrib-keyword symbol))
  (if (slot-boundp object '%all-attributes)
      (setf (getf (cadr (assoc slot-name (slot-value object '%all-attributes))) attrib-keyword) new-value)))

(defmethod (setf slot-attribs) (new-value (object attributes-object) (slot-name symbol))
  (if (slot-boundp object '%all-attributes)
      (setf (cadr (assoc slot-name (slot-value object '%all-attributes))) (rest new-value))))

(defmethod (setf closer-mop:slot-value-using-class) :around (new-value (class attributes-class) (object attributes-object) (slotd attributed-effective-slot))
  (if (and (slot-boundp object '%all-attributes) (listp new-value) (ignore-errors (getf new-value :value/attribs)))
      (let ((value (car (getf new-value :value/attribs)))
	    (attrib-pairs (pairup-list (cadr (getf new-value :value/attribs))))
	    (slot-name (closer-mop:slot-definition-name slotd)))
	(setf (slot-value object slot-name) value)
	(dolist (pair attrib-pairs)
	  (setf (slot-attrib object slot-name (car pair)) (second pair)))
	new-value)
      (call-next-method)))

(defmethod initialize-instance :around ((object attributes-object) &rest initargs)
  (let* ((all-slots (closer-mop:class-slots (class-of object)))
	 (init-pairups (pairup-list initargs))
	 (attribed-pairs nil)
	 (replaced-pairs nil))
    (mapcar (lambda (pair)
	      (cond ((ignore-errors (eq (car (second pair)) :value/attribs))
		     (push (copy-list pair) attribed-pairs))))
	    init-pairups)
    (mapcar (lambda (slot)
	      (mapcar (lambda (pair)
			(declare (special pair))
			(cond ((eq (car (closer-mop:slot-definition-initargs slot)) (first pair))
			       (push (list (first pair) (second (getf (car (rest pair)) :value/attribs))) replaced-pairs))))
		      attribed-pairs))
	    all-slots)
    (setf object (call-next-method))
    (let ((final-attribs
	    (remove nil (mapcar (lambda (slot)
		       (let* ((slot-keyword (car (closer-mop:slot-definition-initargs slot)))
			      (slot-attribs (cadr (assoc (closer-mop:slot-definition-name slot) (slot-value object '%all-attributes))))
			      (to-be-set-attribs (cadr (assoc slot-keyword replaced-pairs)))
			      (to-be-set-value (car (cadadr (assoc slot-keyword attribed-pairs)))))
			 (mapcar (lambda (to-be-set-attrib)
				   (cond ((getf slot-attribs (car to-be-set-attrib))
					  (setf (getf slot-attribs (car to-be-set-attrib)) (cadr to-be-set-attrib))
					  (setf to-be-set-attribs
						(remove (cadr to-be-set-attrib)
							(remove (car to-be-set-attrib) to-be-set-attribs))))))
				 (pairup-list to-be-set-attribs))
			 (if to-be-set-value (setf (slot-value object (closer-mop:slot-definition-name slot)) to-be-set-value))
			 (if (or slot-attribs to-be-set-attribs)
			     (list (closer-mop:slot-definition-name slot) (append slot-attribs to-be-set-attribs)))))
		     all-slots))))
      (setf (slot-value object '%all-attributes) final-attribs))
    object))
