
(defpackage :sexml
  (:use :cl-ppcre :alexandria :contextl-common-lisp)
  (:export :support-dtd :with-compiletime-active-layers :standard-sexml
           :ie-conditionals :xml-doctype))
