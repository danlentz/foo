;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defparameter *paragraph-tags* '(:paragraph :modeline :item :href :text :td)
  "Tags that should be parsed as paragraphs when they occur at the top level.")

(defparameter *subdocument-tags* '(:subdoc :note :sidebar :bullets :link :table :tr)
  "Tags that should be parsed as documents that can contain paragraphs.")

(defparameter *indented-paragraph-tags* '((2 . :blockquote) (4 . :example))
  "Mapping from indentation levels to tag names.")

(defparameter *outline-tag-base* "H"
  "Prefix used to create tag names for Emacs outline header paragraphs.")

(defparameter *invisible* '(:modeline :foo)
  "Tags that are not included in the output of RENDER.")

(defparameter *copyright* "2005, Peter Seibel"
  "Included in copyright notice included in footer of PDF output.")