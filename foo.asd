;;;;; -*-    mode: lisp; syntax: common-lisp; coding: utf-8; base: 10;      -*-
;;;;;
;;;;; fu.asd
;;;;;    parsing is cool
;;;;;
;;;;; Copyright 2009 Dan Lentz, Lentz Intergalactic Softworks
;;;;; Will the third world war keep ``Bosom Buddies'' off the air?
;;;;;
;;;;; Updated:Dan Lentz 2009-Mar-27 16:57:49 EDT
;;;;; Created: Dan Lentz <dan@lentz.com> 2009-03-27
;;;;; 
;;;;; Keywords: asd common-lisp i386-apple-darwin9.6.0
;;;;; Platform: Clozure Common Lisp Version 1.3-RC1-r11847M  (DarwinX8664)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  begin source code  .....................................................

(in-package :common-lisp-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (optimize (debug 3))))

(defpackage :foo.system
  (:use :common-lisp :asdf))

(in-package :foo.system)

(defsystem :foo
  :name "foo is easily conused with fu"
  :version "0.0.1"
  :maintainer "Dan Lentz <danlentz@gmail.com>"
  :description "a language-oriented toolkit"
  :author "Dan Lentz <danlentz@gmail.com>"
  :licence "LGPL"
  :serial t :components
  ((:file "packages")
    (:module "html" :serial t :components ((:file "html")))
    (:module "markdown" :serial t :components ((:file "markdown")))))
    




;;;;  end source code  .......................................................
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Local Variables:
;;  tab-width: 8
;;  fill-column: 78
;;  indent-tabs-mode: nil
;;  comment-column: 50
;;  comment-start: ";; "
;; End: