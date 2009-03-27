(in-package :xml)

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar #'(lambda (name) `(,name (gensym))) names)
     ,@body))

;;; public api

(defvar *pretty* t)
(defvar *xml-output* *standard-output*)
(defvar *xml-pretty-printer* nil)

(defmacro with-xml-output ((stream &key (pretty *pretty*)) &body body)
  `(let ((*xml-output* ,stream)
	 (*pretty* ,pretty))
     ,@body))

(defmacro with-xml-to-file ((file &key (pretty *pretty*)) &body body)
  (with-gensyms (stream)
    `(with-open-file (,stream ,file :direction :output :if-exists :supersede)
       (with-xml-output (,stream :pretty ,pretty)
	 ,@body))))

(defun emit-xml (sexp) (process (get-pretty-printer) sexp))

(defmacro xml (&whole whole &body body)
  (declare (ignore body))
  `(if *pretty*
       (macrolet ((xml (&body body) (codegen-xml (sexp->ops body) t)))
	 (let ((*xml-pretty-printer* (get-pretty-printer))) ,whole))
       (macrolet ((xml (&body body) (codegen-xml (sexp->ops body) nil)))
	 ,whole)))

(defun get-pretty-printer ()
  (or *xml-pretty-printer*
      (make-instance 'xml-pretty-printer
		     :printer (make-instance 'indenting-printer :out *xml-output*))))

(defun codegen-xml (ops pretty)
  (let ((*pretty* pretty))
    `(progn ,@(generate-code (optimize-static-output ops)) nil)))

;;; string escaping

(defparameter *element-escapes* "<>&")
(defparameter *attribute-escapes* "<>&\"'")
(defvar *escapes* *element-escapes*)

(defun escape-char (char)
  (case char
    (#\& "&amp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\' "&apos;")
    (#\" "&quot;")
    (t (format nil "&#~d;" (char-code char)))))

(defun escape (in to-escape)
  (flet ((needs-escape-p (char) (find char to-escape)))
    (with-output-to-string (out)
      (loop for start = 0 then (1+ pos)
	   for pos = (position-if #'needs-escape-p in :start start)
	   do (write-sequence in out :start start :end pos)
	   when pos do (write-sequence (escape-char (char in pos)) out)
	   while pos))))

;;; indenting printer

(defclass indenting-printer ()
  ((out                 :accessor out                 :initarg :out)
   (beginning-of-line-p :accessor beginning-of-line-p :initform t)
   (indentation         :accessor indentation         :initform 0)
   (indenting-p         :accessor indenting-p         :initform t)))

(defun emit (ip string)
  (loop for start = 0 then (1+ pos)
       for pos = (position #\Newline string :start start)
       do (emit/no-newlines ip string :start start :end pos)
       when pos do (emit-newline ip)
       while pos))

(defun emit/no-newlines (ip string &key (start 0) end)
  (indent-if-necessary ip)
  (write-sequence string (out ip) :start start :end end)
  (unless (zerop (- (or end (length string)) start))
    (setf (beginning-of-line-p ip) nil)))

(defun emit-newline (ip)
  (write-char #\Newline (out ip))
  (setf (beginning-of-line-p ip) t))

(defun emit-freshline (ip)
  (unless (beginning-of-line-p ip) (emit-newline ip)))

(defun indent-if-necessary (ip)
  (when (and (beginning-of-line-p ip) (indenting-p ip))
    (loop repeat (indentation ip) do (write-char #\Space (out ip)))
    (setf (beginning-of-line-p ip) nil)))

;;; xml proc interface

(defgeneric raw-string (processor string &optional check-for-newlines))
(defgeneric newline (processor))
(defgeneric freshline (processor))
(defgeneric indent (processor))
(defgeneric unindent (processor))
(defgeneric toggle-indenting (processor))
(defgeneric embed-value (processor value))
(defgeneric embed-code (processor code))

;;; xml pretty printer

(defclass xml-pretty-printer ()
  ((printer :accessor printer :initarg :printer)
   (tab-width :accessor tab-width :initarg :tab-width :initform 2)))

(defmethod raw-string ((pp xml-pretty-printer) string &optional newlines-p)
  (if newlines-p
      (emit (printer pp) string)
      (emit/no-newlines (printer pp) string)))

(defmethod newline ((pp xml-pretty-printer))
  (emit-newline (printer pp)))

(defmethod freshline ((pp xml-pretty-printer))
  (when *pretty* (emit-freshline (printer pp))))

(defmethod indent ((pp xml-pretty-printer))
  (when *pretty*
    (incf (indentation (printer pp)) (tab-width pp))))

(defmethod unindent ((pp xml-pretty-printer))
  (when *pretty*
    (decf (indentation (printer pp)) (tab-width pp))))

(defmethod toggle-indenting ((pp xml-pretty-printer))
  (when *pretty*
    (with-slots (indenting-p) (printer pp)
      (setf indenting-p (not indenting-p)))))

(defmethod embed-value ((pp xml-pretty-printer) value)
  (error "Can't embed values when interpreting. Value: ~s" value))

(defmethod embed-code ((pp xml-pretty-printer) code)
  (error "Can't embed code when interpreting. Code: ~s" code))

;;; ops buffer

(defun make-op-buffer () (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops-buffer) (vector-push-extend op ops-buffer))

;;; compiler

(defclass xml-compiler ()
  ((ops :accessor ops :initform (make-op-buffer))))

(defmethod raw-string ((compiler xml-compiler) string &optional newlines-p)
  (push-op `(:raw-string ,string ,newlines-p) (ops compiler)))

(defmethod newline ((compiler xml-compiler))
  (push-op '(:newline) (ops compiler)))

(defmethod freshline ((compiler xml-compiler))
  (push-op '(:freshline) (ops compiler)))

(defmethod indent ((compiler xml-compiler))
  (push-op '(:indent) (ops compiler)))

(defmethod unindent ((compiler xml-compiler))
  (push-op '(:unindent) (ops compiler)))

(defmethod toggle-indenting ((compiler xml-compiler))
  (push-op '(:toggle-indenting) (ops compiler)))

(defmethod embed-value ((compiler xml-compiler) value)
  (push-op `(:embed-value ,value ,*escapes*) (ops compiler)))

(defmethod embed-code ((compiler xml-compiler) code)
  (push-op `(:embed-code ,code) (ops compiler)))

(defun sexp->ops (body)
  (loop with compiler = (make-instance 'xml-compiler)
       for form in body do (process compiler form)
       finally (return (ops compiler))))

(defun optimize-static-output (ops)
  (let ((new-ops (make-op-buffer)))
    (with-output-to-string (buf)
      (flet ((add-op (op)
	       (compile-buffer buf new-ops)
	       (push-op op new-ops)))
	(loop for op across ops do
	     (ecase (first op)
	       (:raw-string (write-sequence (second op) buf))
	       ((:newline :embed-code :embed-value) (add-op op))
	       ((:indent :unindent :freshline :toggle-indenting)
		(when *pretty* (add-op op)))))
	(compile-buffer buf new-ops)))
    new-ops))

(defun compile-buffer (buf ops)
  "compile a string possibly containing newlines into a sequence of
:raw-string and :newline ops."
  (loop with str = (get-output-stream-string buf)
     for start = 0 then (1+ pos)
     for pos = (position #\Newline str :start start)
     when (< start (length str))
     do (push-op `(:raw-string ,(subseq str start pos) nil) ops)
     when pos do (push-op '(:newline) ops)
     while pos))

(defun generate-code (ops)
  (loop for op across ops collect (apply #'op->code op)))

(defgeneric op->code (op &rest operands))

(defmethod op->code ((op (eql :raw-string)) &rest operands)
  (destructuring-bind (string check-for-newlines) operands
    (if *pretty*
	`(raw-string *xml-pretty-printer* ,string ,check-for-newlines)
	`(write-sequence ,string *xml-output*))))

(defmethod op->code ((op (eql :newline)) &rest operands)
  (if *pretty*
      `(newline *xml-pretty-printer*)
      `(write-char #\Newline *xml-output*)))

(defmethod op->code ((op (eql :freshline)) &rest operands)
  (if *pretty*
      `(freshline *xml-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :indent)) &rest operands)
  (if *pretty*
      `(indent *xml-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :unindent)) &rest operands)
  (if *pretty*
      `(unindent *xml-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :toggle-indenting)) &rest operands)
  (if *pretty*
      `(toggle-indenting *xml-pretty-printer*)
      (error "Bad op when not pretty-printing: ~a" op)))

(defmethod op->code ((op (eql :embed-value)) &rest operands)
  (destructuring-bind (value escapes) operands
    (if *pretty*
	(if escapes
	    `(raw-string *xml-pretty-printer* (escape (princ-to-string ,value) ,escapes) t)
	    `(raw-string *xml-pretty-printer* (princ-to-string ,value) t))
	(if escapes
	    `(write-sequence (escape (princ-to-string ,value) ,escapes) *xml-output*)
	    `(princ ,value *xml-output*)))))

(defmethod op->code ((op (eql :embed-code)) &rest operands)
  (first operands))

;;; xml processor

(defun process (processor form)
  (cond
    ((special-form-p form) (process-special-form processor form))
    ((macro-form-p form)   (process processor (expand-macro-form form)))
    ((sexp-xml-p form)     (process-sexp-xml processor form))
    ((consp form)          (embed-code processor form))
    (t                     (embed-value processor form))))

;;; language syntax

(defun sexp-xml-p (form)
  (or (self-evaluating-p form) (cons-form-p form)))

(defun self-evaluating-p (form)
  (and (atom form)
       (if (symbolp form) (keywordp form) t)))

(defun cons-form-p (form &optional (test #'keywordp))
  (and (consp form)
       (or (funcall test (car form))
	   (and (consp (car form)) (funcall test (caar form))))))

(defun macro-form-p (form)
  (cons-form-p form #'(lambda (x) (and (symbolp x) (get x 'xml-macro)))))

(defun special-form-p (form)
  (and (consp form) (symbolp (car form)) (get (car form) 'xml-special-operator)))

(defun parse-cons-form (sexp)
  (if (consp (first sexp))
      (parse-explicit-attributes-sexp sexp)
      (parse-implicit-attributes-sexp sexp)))

(defun parse-explicit-attributes-sexp (sexp)
  (destructuring-bind ((tag &rest attributes) &body body) sexp
    (values tag attributes body)))

(defun parse-implicit-attributes-sexp (sexp)
  (loop with tag = (first sexp)
     for rest on (rest sexp) by #'cddr
     while (and (keywordp (first rest)) (second rest))
     when (second rest)
       collect (first rest) into attributes and
       collect (second rest) into attributes
       end
       finally (return (values tag attributes rest))))

;;; sexp-xml

(defparameter *block-elements*
  '(:dataset :view :script :method))

(defparameter *paragraph-elements*
  '(:text :htmltext))

(defparameter *inline-elements*
  '(:attribute))

(defparameter *empty-elements*
  '(:br))

(defparameter *js-body-elements*
  '(:script :method))

(defparameter *preserve-whitespace-elements* '(:pre))

(defun self-to-string (self-form)
  (cond
    ((keywordp self-form)
     (princ-to-string (js:symbol-to-js self-form)))
    (t (princ-to-string self-form))))

(defun process-sexp-xml (processor form)
  (if (self-evaluating-p form)
      (raw-string processor (escape (self-to-string form) *escapes*) t)
      (process-cons-sexp-xml processor form)))

(defun process-cons-sexp-xml (processor form)
  (when (string= *escapes* *attribute-escapes*)
    (error "Can't use cons forms in attributes: ~a" form))
  (multiple-value-bind (tag attributes body) (parse-cons-form form)
    (emit-open-tag     processor tag body attributes)
    (emit-element-body processor tag body)
    (emit-close-tag    processor tag body)
    (when (block-element-p tag)
      (newline processor))))

(defun emit-open-tag (processor tag body-p attributes)
  (when (or (paragraph-element-p tag) (block-element-p tag))
    (freshline processor))
  (raw-string processor (format nil "<~a" (symbol-to-js tag)))
  (emit-attributes processor attributes)
  (raw-string processor (if (not body-p) "/>" ">")))

(defun emit-attributes (processor attributes)
  (loop for (k v) on attributes by #'cddr 
       for name = (symbol-to-js k) do
       (raw-string processor (format nil " ~a='" name))
       (if (and (> (length name) 2)
		(string= (subseq name 0 2) "on"))
	   (emit-js processor v nil)
	   (let ((*escapes* *attribute-escapes*))
	     (process processor (if (eql v t) (string-downcase k) v))))
       (raw-string processor "'")))

(defun single-immediate-body-p (body)
  (and (= (length body) 1)
       (self-evaluating-p (first body))))

(defun emit-element-body (processor tag body)
  (unless (or (inline-element-p tag)
	      (single-immediate-body-p body))
    (freshline processor)
    (indent processor))
  (when (preserve-whitespace-p tag)
    (toggle-indenting processor))
  (if (js-body-element-p tag)
      (emit-js processor (cons 'progn body) t)
      (dolist (item body) (process processor item)))
  (when (preserve-whitespace-p tag)
    (toggle-indenting processor))
  (unless (or (inline-element-p tag)
	      (single-immediate-body-p body))
    (unindent processor)
    (freshline processor)))

(defun emit-close-tag (processor tag body-p)
  (unless (not body-p)
    (raw-string processor (format nil "</~a>" (symbol-to-js tag))))
  (unless (inline-element-p tag)
    (freshline processor)))

(defun block-element-p (tag) (find tag *block-elements*))

(defun inline-element-p (tag) (find tag *inline-elements*))

(defun js-body-element-p (tag) (find tag *js-body-elements*))

(defun paragraph-element-p (tag) (find tag *paragraph-elements*))

(defun empty-element-p (tag) (find tag *empty-elements*))

(defun preserve-whitespace-p (tag) (find tag *preserve-whitespace-elements*))

;;; special ops

(defmacro define-xml-special-operator (name (processor &rest other-parameters) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name 'xml-special-operator)
	   (lambda (,processor ,@other-parameters) ,@body))))

(defun remove-xml-special-operator (name)
  (setf (get name 'xml-special-operator) nil))

(defun process-special-form (processor form)
  (apply (get (car form) 'xml-special-operator) processor (rest form)))

;;; macros

(defmacro define-xml-macro (name (&rest args) &body body)
  (multiple-value-bind (attribute-var args)
      (parse-xml-macro-lambda-list args)
    (if attribute-var
	(generate-macro-with-attributes name attribute-var args body)
	(generate-macro-no-attributes name args body))))

(defun generate-macro-with-attributes (name attribute-args args body)
  (with-gensyms (attributes form-body)
    (if (symbolp attribute-args)
	(setf attribute-args `(&rest ,attribute-args)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'xml-macro-wants-attributes) t)
       (setf (get ',name 'xml-macro)
	     (lambda (,attributes ,form-body)
	       (destructuring-bind (,@attribute-args) ,attributes
		 (destructuring-bind (,@args) ,form-body
		   ,@body)))))))

(defun generate-macro-no-attributes (name args body)
  (with-gensyms (form-body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'xml-macro-wants-attributes) nil)
       (setf (get ',name 'xml-macro)
	     (lambda (,form-body)
	       (destructuring-bind (,@args) ,form-body ,@body))))))

(defun parse-xml-macro-lambda-list (args)
  "Parse a lambda list that can include the &attributes lambda-list-keyword."
  (let ((attr-cons (member '&attributes args)))
    (values
     (cadr attr-cons)
     (nconc (ldiff args attr-cons) (cddr attr-cons)))))

(defun expand-macro-form (form)
  (if (or (consp (first form))
	  (get (first form) 'xml-macro-wants-attributes))
      (multiple-value-bind (tag attributes body) (parse-cons-form form)
	(funcall (get tag 'xml-macro) attributes body))
      (destructuring-bind (tag &body body) form
	(funcall (get tag 'xml-macro) body))))

(defun remove-xml-macro (name)
  (setf (get name 'xml-macro) nil))

;;; special forms

(define-xml-special-operator :print (processor form)
  (cond ((self-evaluating-p form)
	 (warn "Redundant :print of self-evaluating form ~s" form)
	 (process-sexp-xml processor form))
	(t (embed-value processor form))))

(define-xml-special-operator :format (processor &rest args)
  (if (every #'self-evaluating-p args)
      (process-sexp-xml processor (apply #'format nil args))
      (embed-value processor `(format nil ,@args))))

(define-xml-special-operator :progn (processor &rest body)
  (loop for exp in body do (process processor exp)))

(define-xml-special-operator :noescape (processor &rest body)
  (let ((*escapes* nil))
    (loop for exp in body do (process processor exp))))

(define-xml-special-operator :attr (processor &rest body)
  (let ((*escapes* *attribute-escapes*))
    (loop for exp in body do (process processor exp))))

(define-xml-special-operator :newline (processor)
  (newline processor))

(define-xml-special-operator :xml-proc (processor tag &rest args)
  (raw-string processor (format nil "<?~a" (symbol-to-js tag)))
  (emit-attributes processor args)
  (raw-string processor "?>")
  (newline processor))

(defun emit-js (processor js-code &optional (newlines t))
  (let ((strs (js:js-to-statement-strings (js:js-compile js-code) 0)))
    (do ((strs strs (cdr strs)))
	((null strs))
      (raw-string processor (escape (first strs) *escapes*) nil)
      (when (not (null (cdr strs)))
	(if newlines
	    (newline processor)
	    (raw-string processor " "))))))

(define-xml-special-operator :js (processor &rest body)
  (emit-js processor (cons 'progn body) (equal *escapes* *attribute-escapes*)))

(define-xml-special-operator :constraint (processor when &rest body)
  (raw-string processor "$")
  (when when
    (raw-string processor (symbol-to-js when)))
  (raw-string processor "{")
  (emit-js processor (first body) (equal *escapes* *attribute-escapes*))
  (raw-string processor "}"))