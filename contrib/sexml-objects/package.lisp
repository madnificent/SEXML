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
	   get-template
	   setup-sexml-objects))

