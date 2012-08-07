
CL-VALIDATIONS

Allows validating CLOS objects by defining a validation for each slot
that needs validation via slot attributes. validators can be regex or
functions. for info about slot-attributes see the readme for
cl-attribs at:

https://github.com/farzadbekran/cl-attribs


EXAMPLE

CL-VALIDATIONS> 
(defclass test-class (attributes-object)
  ((email :accessor email :initarg :email :initform nil 
	  :attributes (:validation '(:test "^[_a-zA-Z0-9-]+(\.[_a-zA-Z0-9-]+)*@[a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*\.(([0-9]{1,3})|([a-zA-Z]{2,3})|(aero|coop|info|museum|name))$" :error-msg "Invalid Email Address!")))
   (username :accessor username :initarg :username :initform nil
	     :attributes (:validation `(:test ,(lambda (x) (> (length x) 0)) :error-msg "User-name Must be Entered!"))))
  (:metaclass attributes-class))

#<ATTRIBUTES-CLASS TEST-CLASS>

;define a class inheriting from attributes-object, with
;attributes-class as the metaclass. the email slot has a regex for as
;its validator and a error-msg to be returned when its not valid. the
;username slot has a function as validator and a error-msg too. note
;that these can be set on the fly too, useing a form like this:

;(setf (username obj) `(:value/attribs ("" (:validation (:test ,#'stringp :error-msg "username is not a string!")))))


CL-VALIDATIONS> (defparameter obj (make-instance 'test-class :username "foo" :email "foo@bar.com"))
OBJ

;you can validate a single slot like this:
CL-VALIDATIONS> (slot-validp obj 'username)
T
NIL

;the first value means the slot value is valid, the second value is
;used to return the error message if the value is not valid

;you can also validate the whole object like this:
CL-VALIDATIONS> (object-validp obj)
T
NIL

;now lets try with some wrong values
CL-VALIDATIONS> (setf (email obj) "foobar@baz")
"foobar@baz"
CL-VALIDATIONS> (setf (username obj) "")
""

;if we validate the object we get:
CL-VALIDATIONS> (object-validp obj)
NIL
((USERNAME "User-name Must be Entered!") (EMAIL "Invalid Email Address!"))

;the first value means the object is not valid, the second is a alist
of the invalid slots name and error messages


-----------------------------------------------------------------------

Contact Info

If u find bugs or have any idea about making cl-validations better
please contact me at:

farzadbekran@gmail.com

-----------------------------------------------------------------------

Copyright (c) 2012 Farzad Bekran


Permission is hereby granted, free of charge, to any person obtaining a copy of this
software and associated documentation files (the "Software"), to deal in the Software
 without restriction, including without limitation the rights to use, copy, modify, 
merge, publish, distribute, sublicense, and/or sell copies of the Software, and to 
permit persons to whom the Software is furnished to do so, subject to the following 
conditions:

The above copyright notice and this permission notice shall be included in all copies 
or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A 
PARTICULAR PURPOSE AND NONINFRINGEMENT. 

IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES
 OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE 
SOFTWARE.
