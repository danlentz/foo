(in-package :cl-user)

(defpackage :js
  (:use :common-lisp)
  (:export
   #|
   ;; addition js symbols
   #:new

   ;; literals
   #:t
   #:nil
   #:this
   #:false
   #:undefined

   ;; keywords
   #:break
   #:continue

   ;; array literals
   #:array
   #:list
   #:aref
   #:make-array

   ;; operators
   #:! #:not #:~
   #:* #:/ #:%
   #:+ #:-
   #:<< #:>>
   #:>>>
   #:< #:> #:<= #:>=
   #:in
   #:eql #:== #:!= #:=
   #:=== #:!==
   #:&
   #:^
   #:\|
   #:\&\& #:and
   #:\|\| #:or
   #:>>= #:<<=
   #:*= #:/= #:%= #:+= #:\&= #:^= #:\|= #:~=
   #:++ #:--
   #:1+ #:1-
   #:incf #:decf

   ;; body forms
   #:progn

   ;; function definition
   #:defun
   #:lambda
   
   ;; object literals
   #:create
   #:slot-value
   #:with-slots

   ;; macros
   #:macrolet
   #:symbol-macrolet

   ;; lisp eval
   #:lisp

   ;; if
   #:if
   #:when
   #:unless

   ;; single argument statements
   #:return
   #:throw

   ;; single argument expressions
   #:delete
   #:void
   #:typeof
   #:instanceof
   #:new

   ;; assignment
   #:setf

   ;; variables
   #:defvar
   #:let

   ;; iteration
   #:do
   #:dotimes
   #:dolist
   #:doeach
   #:while

   ;; with
   #:with

   ;; case
   #:case
   #:default

   ;; try throw catch
   #:try

   ;; regex literals
   #:regex

   ;; conditional compilation (IE)
   #:cc-if

   ;; math library
   #:floor
   #:random

   ;; xmlhttprequest
   #:xml-http-request
   #:make-xml-http-request

   ;; html generator for javascript
   #:html
   |#

   ;; compiler
   #:js-compile
   #:js
   #:js-inline
   #:js-file
   #:js-script
   #:js-to-strings
   #:js-to-statement-strings
   #:js-to-string
   #:js-to-line
   #:symbol-to-js
   
   ;; CSS
   #:css
   #:css-to-string
   #:css-inline
   #:css-file

   ))

(defpackage :xml
  (:use :common-lisp :js)
  (:export :with-xml-output
	   :with-xml-to-file
	   :define-xml-macro
	   :xml
	   :&attributes))
  
#+aserve
(defpackage :klammerscript.examples
  (:use :cl :cl-user
        :js
        :net.aserve
        :net.html.generator))

(defpackage :laszlo
  (:use :common-lisp :js :xml :net.aserve)
  (:export :laszlo-file
	   :publish-xml-handler
	   :with-query-params
	   :xml-http-output
	   :xml-to-http
	   :update-xml-class
	   :insert-xml-class))

(defpackage :laszlo.examples
  (:use :common-lisp :laszlo :bknr.impex :bknr.indices :bknr.datastore))