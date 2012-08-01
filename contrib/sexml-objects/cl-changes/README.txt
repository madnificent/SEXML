cl-changes

Info

Detects changes of the slot values and sets its %changedp slot to t
and calls the function in the on-change slot, if the changed slot has an attribute of :on-change, that function will be called too.
the format for the slot's :on-change attribute function is (lambda (object)...)
the format for the objects %on-change slot's function is (lambda (object slot-name)...)

-----------------------------------------------------------------------
Example use

CL-CHANGES>
(defclass test (change-sensitive-object)
  ((slot-a     :accessor slot-a        :initarg :slot-a    :initform nil 
	       :attributes (:on-change (lambda (object) 
					 (format t "slot-a of ~a has changed~%" object))))
   (slot-b     :accessor slot-b        :initarg :slot-b    :initform nil)
   (%on-change :accessor %on-change :initarg :%on-change 
	       :initform (lambda (object slot) (format t "~a's ~a changed~%" object slot))))
  (:metaclass attributes-class))

#<ATTRIBUTES-CLASS TEST>

CL-CHANGES> (defparameter obj (make-instance 'test :slot-a "value 1"))
#<TEST {BAEDA39}>'s SLOT-B changed
#<TEST {BAEDA39}>'s %ALL-ATTRIBUTES changed
OBJ

-----------------------------------------------------------------------

;;setting a slot value will trigger the appropriate on-change functions

CL-CHANGES> (setf (slot-a obj) "val2")
#<TEST {BAEDA39}>'s SLOT-A changed  ;;this is from %on-change slots function
slot-a of #<TEST {BAEDA39}> has changed  ;;this is from slot-a's :on-change attribute's function
"val2"

-----------------------------------------------------------------------

;;check to see if the object has changed

CL-CHANGES> (%changedp obj)
T

-----------------------------------------------------------------------

;;set it's changed status to nil

CL-CHANGES> (setf (%changedp obj) nil)
NIL

-----------------------------------------------------------------------

Contact Info

If u find bugs or have any idea about making cl-changes better please contact me at

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
