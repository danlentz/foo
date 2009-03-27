(in-package :laszlo)

(defclass js-xml-processor ()
  ((ret-name :initarg :ret-name :reader js-xml-ret-name)
   (tmp-counter :initform 0 :accessor js-xml-tmp-counter)))

(defmethod next-xml-tmp-var ((proc js-xml-processor))
  (let ((ret (intern (format nil "~A-~A" (js-xml-ret-name proc) (js-xml-tmp-counter proc))
		     (symbol-package (js-xml-ret-name proc)))))
    (incf (js-xml-tmp-counter proc))
    ret))

(defun js-form-p (form)
  (not (xml::cons-form-p form)))

(defun simple-body-p (body)
  (and body (every #'js-form-p body)))

(defun simple-body-to-code (body)
  `(new (-lz-data-text
	 ,(if (= (length body) 1)
	      (first body)
	      `(+ ,@body)))))

(defun simple-body-to-string (body)
  (if (= (length body) 1)
      (first body)
      `(+ ,@body)))

(defun data-elt-code (tag &optional attrs body)
  (let ((res `(,tag -lz-data-element)))
    (when (or body attrs)
      (push `(create ,@attrs) res))
    (when body
      (push body res))
    `(new ,(reverse res))))

;; returns xml code to build node
;; diese child-var geschichte ist noch nicht so elegant XXX
(defmethod form-to-code ((proc js-xml-processor) form &optional var)
  (cond ((js-form-p form)
	 (let ((code `(new -lz-data-text ,form)))
	   (if var
	       (values (list `(defvar ,var ,code)) var)
	       (values (list code) nil))))
	((xml::cons-form-p form)
	 (multiple-value-bind (tag attributes body) (xml::parse-cons-form form)
	   (cond ((simple-body-p body)
		  (let ((code #+nil(data-elt-code (symbol-to-js tag) attributes
						  `(array ,(simple-body-to-code body)))
			      `(make-simple-node ,(symbol-to-js tag) (create ,@attributes)
						 ,(simple-body-to-string body))))
		    (if var
			(values (list `(defvar ,var ,code)) var)
			(values (list code) nil))))
		 (t (let* ((var (or var (next-xml-tmp-var proc)))
			   (res (list)))
		      (push `(defvar ,var
			       ,(data-elt-code (symbol-to-js tag) attributes)) res)
		      (dolist (child body)
			(if (js-form-p child)
			    (push `(.append-child ,var ,(simple-body-to-code (list child))) res)
			    (multiple-value-bind (code child-var) (form-to-code proc child)
			      (if child-var
				  (progn
				    (push (first code) res)
				    (push `(.append-child ,var ,child-var) res)
				    (dolist (line (rest code))
				      (push line res)))
				  (if (= (length code) 1)
				      (push `(.append-child ,var ,(first code)) res)
				      (progn
					(setf child-var (next-xml-tmp-var proc))
					(push `(defvar ,child-var ,(first code)) res)
					(push `(.append-child ,var ,child-var) res)
					(dolist (line (cdr code))
					  (push line res))))))))
		      (values (nreverse res) var))))))
	(t (error "foobar"))))

(defun process-xml-forms (xml var)
  (let ((proc (make-instance 'js-xml-processor :ret-name var)))
    `(progn ,@(form-to-code proc xml var))))

(js::define-js-compiler-macro xml (ret-var xml-form)
  (js:js-compile (process-xml-forms xml-form ret-var)))