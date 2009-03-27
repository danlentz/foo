;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo)

(define-html-macro :comment (&body body)
  `(:progn
    (:noescape "<!-- ")
    ,@body
    (:noescape " -->")))

(define-html-macro :character (name)
  `(:noescape (:format ,(if (numberp name) "&#~d;" "&~(~a~);") ,name)))

(define-html-macro :pi (name &rest attrs)
  `(:progn 
    (:noescape (:format "<?~a ~@{~(~a~)=\"~a\"~^ ~}?>" ,name ,@attrs))
    (:newline)))

(define-html-macro :doctype (name type id url)
  `(:progn
    (:noescape (:format "<!DOCTYPE ~a ~a \"~a\" \"~a\">" ,name ,type ,id ,url))
    (:newline)))

(define-html-macro :xhtml (&body body)
  (assert *xhtml*)
  `(:progn
     (:pi "xml" :version 1.0 :encoding "UTF-8")
    (:doctype "html" "PUBLIC" "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
    ((:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en")
     ,@body)))

(define-html-macro :css-stylesheet (href)
  `(:link :rel "stylesheet" :type "text/css" :href ,href))

;(define-html-macro :css (&rest values)
;  `(:attribute (:format "~@{~(~a~): ~a;~^ ~}" ,@values)))

(define-html-macro :css (&body body)
  `((:style :type "text/css")
    (:comment (css-style ,@body))))

