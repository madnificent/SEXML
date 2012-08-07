;;package.lisp

(defpackage sexml-objects
  (:use #:cl #:sexml #:cl-attribs #:cl-changes #:cl-binds)
  (:export widget
	   widget-classes
	   %parent
	   %id
	   %render-func
	   %children
	   update-children-parent
	   map-widget
	   render-markup
	   get-html-attributes
	   widgetp
	   add-child
	   remove-child
	   define-ui
	   define-template
	   define-template-from-file
	   get-template
	   with-template
	   setup-sexml-objects))

