;;;; package.lisp

(in-package :cl-user)

(defpackage #:cl-validations
  (:use #:cl #:cl-attribs #:cl-ppcre)
  (:export object-validp slot-validp :validation :test :error-msg))

