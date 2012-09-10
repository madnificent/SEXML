;;;; package.lisp

(defpackage #:cl-binds
  (:use #:cl #:cl-attribs)
  (:export bindable-object
	   %bound-object))

