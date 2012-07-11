;;;; package.lisp

(defpackage #:cl-attribs
  (:use #:cl)
  (:export attributes-class
	   attributes-object
	   slot-attrib
	   slot-attribs
	   attributed-direct-slot
	   attributed-effective-slot
	   attributed-slot-p
	   %all-attributes))

