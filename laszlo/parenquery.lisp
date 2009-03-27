;;; parenquery -- simple chaining JS method calls for JQuery

;;; instead of manual:
;; PARENQUERY> (ps (chain foo bar baz))
;; "foo.bar.baz;"
;; PARENQUERY> (ps (chain foo bar (baz)))
;; "foo.bar.baz();"
;; PARENQUERY> (ps (chain foo (bar 1 2 3) (baz)))
;; "foo.bar(1, 2, 3).baz();"
;; PARENQUERY> (ps (chain (me.next)
;;                        (show "slow" (lambda ()
;;                                       (chain ($ this) (filter "li") (css "display" "list-item"))
;;                                       (chain ($ this) (children) (eq 0) (focus))
;;                                       (chain ($ elt) (fade-out))
;;                                       (chain ($ this) (children) (filter ".__more") (fade-in))))))
;; "me.next().show('slow', function () {
;;     $(this).filter('li').css('display', 'list-item');
;;     $(this).children().eq(0).focus();
;;     $(elt).fadeOut();
;;     $(this).children().filter('.__more').fadeIn();
;; });"

(defpackage #:parenquery
  (:use #:common-lisp #:parenscript)
  (:export #:chain))
(in-package #:parenquery)

(defun %chain-1 (first second)
  (if (listp second)
      `((slot-value ,first ',(first second)) ,@(rest second))
      `(slot-value ,first ',second)))

(defun %chain (first second &rest rest)
  (if rest
      (apply #'%chain (%chain-1 first second) rest)
      (%chain-1 first second)))

(defpsmacro chain (first second &rest rest)
  "Chain methods, as for JQuery (jquery.org)"
  (apply #'%chain first second rest))

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for details.
