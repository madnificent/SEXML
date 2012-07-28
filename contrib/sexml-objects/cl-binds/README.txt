
CL-BINDS

Allows binding object A to object B so the object A's slots
will act like they are from object B, reading and writing to object B.
This is usefull when you have something like a text box that should display 
and edit a slot like username from an object like a user object. To ease up 
things you can simply bind the value slot of the text box to username slot
of the user object and its done. no need to do it by hand!

-----------------------------------------------------------------------

How To Use

All you have to do is define a class and inherit from
"bindable-object".

Also your class must have a metaclass of "attributes-class".

A slot called %bound-object will be inherited from bindable-object
that you will use to specify the object to bind to.

To specify which slot is bound to the other object's slot you will use
an attribute called :bound-slot-name.

To have control over the values that get read and written, you can
specify :read and :write functions via attributes to be called with
one argument that is the value that has been read, or the value that
is going to be written. This allows transformations to be done to the
values. like when you have a time-stamp object bound to some slot and
you want the bound object to get only a string containing the year.

Example:

CL-BINDS>
(defclass text-box (bindable-object)
  ((value :accessor value :initform nil :initarg :value 
          :attributes (:bound-slot-name 'username 
					:reader (lambda (x) (concatenate 'string x " read-value"))
					:writer (lambda (x) (concatenate 'string x " written-value"))))
   (slot-b :accessor slot-b :initform nil :initarg :slot-b))
  (:metaclass attributes-class))

==> #<ATTRIBUTES-CLASS TEST>

CL-BINDS> 
(defclass user () 
   ((username :accessor get-username :initarg :username :initform "foo")
    (password :accessor get-password :initarg :password :initform "bar")))

==> #<STANDARD-CLASS USER>

CL-BINDS> (defparameter bindable (make-instance 'text-box))
BINDABLE

CL-BINDS> (defparameter user-object (make-instance 'user))
USER-OBJECT

CL-BINDS> (setf (%bound-object bindable) user-object)
#<USER {B8FB781}>

CL-BINDS> (value bindable)
"foo read-value" ;; this is read from the user-objects username slot as expected.
     		 ;; note that the value returned is the result of calling the 
		 ;; function specified in :reader attribute of the bindable-object

CL-BINDS> (setf (value bindable) "set after bind")
"set after bind" ;; this will set the username slot of the user-object as value slot 
                 ;; is bound to username, the value that is set will be the result 
		 ;; of calling the function specified in :writer attribute of the 
		 ;; bindable-object 

CL-BINDS> (get-username user-object)
"set after bind written-value"

CL-BINDS> (setf (%bound-object bindable) nil)
NIL  ;; binding removed, everything is back to normal now.

-----------------------------------------------------------------------

Contact Info

If u find bugs or have any idea about making cl-binds better please contact me at

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
