;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.markup)

(defparameter *tag-translations* `((:example     . :pre)
				   (:sidebarhead . (:div :class "sidebarhead"))
				   (:sidebar     . (:div :class "sidebar"))
				   (:note        . (:div :class "note"))
				   (:note-ref    . :sup)
				   (:bullets     . :ul)
				   (:item        . :li)
				   (:url         . htmlize-url)
				   (:link        . htmlize-link)))

(defun htmlize-url (tag body)
  (declare (ignore tag))
  `(:a :href ,@body ,@body))

(defun htmlize-link (tag body)
  (declare (ignore tag))
  (destructuring-bind ((hreftag href) (texttag text)) body
    (declare (ignore hreftag texttag))
    `(:a :href ,href ,text)))

(defmethod render-as ((type (eql :html)) sexp file)
  (with-html-to-file (file)
    (emit-html (make-foo-html sexp))))

(defun make-foo-html (sexp)
  (multiple-value-bind (body notes) (extract-notes sexp)
    `(:html
       (:head 
	(:title ,(find-first-h1 sexp))
	(:link :rel "stylesheet" :type "text/css" :href "style.css"))
       (:body
	,@(translate-tags body)
	,@(when notes
		`((:hr)
		  ,@(loop for note in (translate-tags notes) 
		       for number from 1 
			 do (push `(:sup ,(format nil "~d. " number)) (cdadr note))
			 collect note)))))))
		  
(defun find-first-h1 (parse)
  (labels ((walk (thing)
	     (when (consp thing)
	       (destructuring-bind (tag &rest body) thing
		 (if (eql tag :h1)
		   (return-from find-first-h1 (car body))
		   (dolist (child body)
		     (walk child)))))))
    (dolist (thing parse)
      (walk thing))))

(defun translate-tag (element)
  (if (atom element)
    element
    (destructuring-bind (tag &rest body) element
      (let ((translation (cdr (assoc tag *tag-translations*))))
	(etypecase translation
	  (null
	   (cons tag (translate-tags body)))
	  (keyword
	   (cons translation (translate-tags body)))
	  (cons
	   (cons translation (translate-tags body)))
	  (symbol
	   (funcall translation tag body)))))))

(defun translate-tags (list)
  (mapcar #'translate-tag list))

