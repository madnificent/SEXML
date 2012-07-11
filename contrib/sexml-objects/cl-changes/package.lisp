;;;; package.lisp

(defpackage #:cl-changes
  (:use #:cl #:cl-attribs)
  (:export change-sensitive-object %on-change %changedp))

