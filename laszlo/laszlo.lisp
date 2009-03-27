(in-package :laszlo)

;;; laszlo macros

(define-xml-macro :jsmethod (name args &rest body)
  `((:method :name ,(symbol-to-js name)
	     :args ,(format nil "~{~a~^,~}" (mapcar #'symbol-to-js args)))
    ,@body))

(define-xml-macro :jsevent (name args &rest body)
  `((:method :event ,(symbol-to-js name)
	     :args ,(format nil "~{~a~^,~}" (mapcar #'symbol-to-js args)))
    ,@body))

(defmacro laszlo-file (filename &body body)
  (let ((fname (gensym)))
    `(let ((,fname ,filename
;             (format nil "C:/Program Files/OpenLaszlo Server 3.1.1/Server/lps-3.1.1/my-apps/~a" filename)
             ))
       (with-xml-to-file (,fname)
	 (xml ,@body))
       (princ #\Newline)
       (with-open-file (s ,fname :direction :input)
	 (loop for line = (read-line s nil)
	      for loc from 1
	      while line
	      do (format t "~A: " loc)
	      (princ line) (princ #\Newline)))
       (format t "~%Written xml to ~s~%" ,fname)
       )))

(defun start-laszlo ()
  (net.aserve:start :port 8081))

