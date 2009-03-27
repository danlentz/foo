;;;;; -*-    mode: lisp; syntax: common-lisp; coding: utf-8; base: 10;      -*-
;;;;;
;;;;; foo.lisp
;;;;;    foo
;;;;;
;;;;; Copyright 2009 Dan Lentz, Lentz Intergalactic Softworks
;;;;; Yow!  Legally-imposed CULTURE-reduction is CABBAGE-BRAINED!
;;;;;
;;;;; Updated:Dan Lentz 2009-Mar-27 17:00:24 EDT
;;;;; Created: Dan Lentz <dan@lentz.com> 2009-03-06
;;;;; 
;;;;; Keywords: lisp common-lisp i386-apple-darwin9.6.0
;;;;; Platform: Clozure Common Lisp Version 1.3-RC1-r11790M  (DarwinX8664)
;;;;;
;;;;;
;;;;  begin source code  .....................................................

(in-package :cl-user)

(defpackage :foo.printer
  (:nicknames :foo.pp :pp)
  (:use :common-lisp)
  (:export :raw-string :newline :freshline :indent :unindent
    :toggle-indenting :embed-value :embed-code
    :get-pretty-printer))

(defpackage :foo.html
  (:nicknames :html)
  (:use :common-lisp :foo.printer)
  (:export :*block-elements* :*paragraph-elements* :with-html-output
    :with-html-to-file :in-html-style :define-html-macro
    :define-html-special-operator :process-special-form
    :html :emit-html :cons-form-p :parse-cons-form
    :&attributes))

(defpackage :foo.css
  (:nicknames :css)
  (:use :common-lisp :foo.html :foo.printer)
  (:export :define-css-macro :css  :emit-css))

(defpackage :foo.markdown
  (:nicknames :foo.md :md)
  (:use :cl :cl-ppcre :foo.html :foo.css :foo.printer)
  (:export :markdown-to-sexp :markdown-file))

(defpackage :foo.markup (:use))

(defpackage :foo.js
  (:use :common-lisp :foo.html :foo.printer)
  (:export :javascript-gensym :emit-javascript :compile-javascript
    :define-javascript-macro))

(defpackage :foo.xml
  (:use :common-lisp :foo.html :foo.printer)
  (:export :xml :emit-xml))

(defpackage :foo.lzx
  (:use :common-lisp :foo.html :foo.printer)
  (:export :xml :emit-xml))







(defpackage :foo.tokens (:use))

;;;;;
;; Local Variables:
;;  tab-width: 8
;;  fill-column: 78
;;  indent-tabs-mode: nil
;;  comment-column: 50
;;  comment-start: ";; "
;; End:
;;;;;