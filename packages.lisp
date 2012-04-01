
(defpackage :sexml
  (:use :cl-ppcre :alexandria :contextl-common-lisp :macroexpand-dammit)
  (:export :support-dtd :with-compiletime-active-layers :standard-sexml
           :ie-conditionals :xml-doctype))
