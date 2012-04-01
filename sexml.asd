
(asdf:defsystem :sexml
  :name "S-Expressions for XML generation"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "s-expressions for xml is a library which provides a sugar-sweet s-expression syntax for spitting out xml documents based on a DTD"
  :depends-on (cl-ppcre alexandria cxml contextl macroexpand-dammit)
  :serial t
  :components ((:file "packages")
               (:file "sexml")))
