;;package.lisp

(defpackage sexml-objects
  (:use #:cl #:sexml #:cl-attribs #:cl-changes #:cl-binds)
  (:export widget %parent %id %render-func %children))

