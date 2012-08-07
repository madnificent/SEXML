
(asdf:defsystem sexml-objects
  :version "0"
  :description "Provides CLOS objects for sexml"
  :maintainer "Farzad <farzadbekran@gmail.com>"
  :author "Farzad <farzadbekran@gmail.com>"
  :licence "Free"
  :depends-on (sexml)
  :serial t
  :components ((:static-file "README")
	       (:module cl-attribs
			:serial t
			:components
			((:static-file "README.txt" :pathname "README.txt")
			 (:file "package")
			 (:file "cl-attribs")))
	       (:module cl-binds
			:serial t
			:components
			((:static-file "README.txt" :pathname "README.txt")
			 (:file "package")
			 (:file "cl-binds")))
	       (:module cl-changes
			:serial t
			:components
			((:static-file "README.txt" :pathname "README.txt")
			 (:file "package")
			 (:file "cl-changes")))
	       (:module cl-validations
			:serial t
			:components
			((:static-file "README.txt" :pathname "README.txt")
			 (:file "package")
			 (:file "cl-validations")))
	       (:static-file "sexml-objects.asd" :pathname "sexml-objects.asd")
	       (:file "package")
	       (:file "sexml-objects"))
  ;; :long-description ""
  )
