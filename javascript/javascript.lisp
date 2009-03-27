;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :com.gigamonkeys.foo.javascript)

;;; Javascript processor

;;; Hmmmm. Might be useful to support symbol macros.

(defun process-javascript (processor form statement-or-expression)
  (cond
    ((javascript-special-form-p form)
     (process-javascript-special-form processor form statement-or-expression))
    ((javascript-macro-form-p form)
     (process-javascript processor (expand-javascript-macro-form form) statement-or-expression))
    (t
     (process-sexp-javascript processor form statement-or-expression))))

(defun javascript-special-form-p (form)
  (and (consp form) (symbolp (car form)) (get (car form) 'javascript-special-operator)))

(defun javascript-macro-form-p (form)
  (and (consp form) (symbolp (car form)) (get (car form) 'javascript-macro)))

(defun process-javascript-special-form (processor form statement-or-expression)
  (process-special-form processor form)
  (when (eql (special-op-type (car form)) :expression)
    ;; The special form is naturally an expression but if it is being
    ;; proceessed as a statement then we need to tack on a
    ;; semicolon. If it's already a statement then it will have taken
    ;; care of emitting any necessary semicolon.
    (maybe-semicolon processor statement-or-expression)))

(defun process-sexp-javascript (processor form statement-or-expression)
  (if (consp form)
      (destructuring-bind (name &rest arguments) form
	(process-javascript processor name :expression)
	(raw-string processor "(")
	(loop for (arg . rest) on arguments do 
	     (process-javascript processor arg :expression)
	   when rest do (raw-string processor ", "))
	(raw-string processor ")"))
      (process-javascript-scalar processor form))
  (maybe-semicolon processor statement-or-expression))

(defun maybe-semicolon (processor statement-or-expression)
  (ecase statement-or-expression
    (:statement (raw-string processor ";"))
    (:expression)))

(defun process-javascript-scalar (processor value)
  ;; This is where better smarts about translating Lisp values to
  ;; Javascript syntax goes. (E.g. (foo x 123.4d0) doesn't work
  ;; because this function will generate 123.4d0 in the Javascript
  ;; output.
  (etypecase value
    (string (raw-string processor (format nil "~s" value)))
    (symbol (raw-string processor (format nil "~a" value)))
    (number (raw-string processor (format nil "~a" value)))))

(defmacro define-javascript-macro (name (&rest args) &body body)
  (with-gensyms (form-body)
    `(setf (get ',name 'javascript-macro)
	   (lambda (,form-body)
	     (destructuring-bind (,@args) ,form-body ,@body)))))

(defun expand-javascript-macro-form (form)
  (destructuring-bind (tag &body body) form
    (funcall (get tag 'javascript-macro) body)))

(defmacro define-javascript-special-operator (name statement-or-expression (processor &rest other-parameters) &body body)
  "Special ops that are always statements are responsible for
outputting their own semicolon if necessary. This allows
statements such as blocks to *not* emit a semicolon."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-html-special-operator ,name (,processor ,@other-parameters)
       (macrolet ((out (&rest stuff)
		    `(progn ,@(compile-special-op-body ',processor stuff)))
		  (emit (thing)
		    `(raw-string ,',processor ,thing)))
	 (flet ((statement (thing)
		  (process-javascript ,processor thing :statement))
		(expression (thing)
		  (process-javascript ,processor thing :expression))
		(name (thing)
		  (raw-string ,processor (symbol-name thing))))
	   (out ,@body))))
     (setf (get ',name 'javascript-special-operator) ,statement-or-expression)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-special-op-body (processor body) 
    (loop for thing in body collect
	 (etypecase thing
	   (string `(raw-string ,processor ,thing))
	   (cons thing)
	   (keyword
	    (ecase thing
	      (:newline `(newline ,processor))
	      (:freshline `(freshline ,processor))
	      (:indent `(indent ,processor))
	      (:unindent `(unindent ,processor))))))))

(defun special-op-type (name) 
  (get name 'javascript-special-operator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operators -- special operators that produce expressions.

(defmacro define-unary-ops (&rest ops)
  `(progn
     ,@(loop for op in ops collect
	    `(define-javascript-special-operator ,op :expression (processor expr)
	       ,(format nil "~(~a~) " op) (expression expr)))))

(defmacro define-binary-ops (&rest ops)
  `(progn
     ,@(loop for op in ops collect
	    `(define-javascript-special-operator ,op :expression (processor &rest expressions)
	       "("
	       (loop for (e . rest) on expressions
		  do (expression e)
		  when rest do (out ,(format nil " ~(~a~) " op)))
	       ")"))))

(defmacro define-assignment-ops (&rest ops)
  `(progn
     ,@(loop for op in ops collect
	    `(define-javascript-special-operator ,op :expression (processor lvalue rvalue)
	       (process-javascript processor lvalue :expression)
	       (raw-string processor ,(format nil " ~a " (symbol-name op)))
	       (process-javascript processor  rvalue :expression)))))

(define-javascript-special-operator array :expression (processor &rest elements)
  "["
  (loop for (e . rest) on elements 
     do (expression e)
     when rest do (out ", "))
  "]")

(define-javascript-special-operator object :expression (processor &rest elements)
  "{ "
  (loop for (key value . rest) on elements by #'cddr
     do (out (name key) " : " (expression value))
     when rest do (out ", "))
  " }")

(define-javascript-special-operator @ :expression (processor expr &rest slots)
  (expression expr)
  (loop for slot in slots do
       (if (symbolp slot)
	 (out "." (name slot))
	 (out "[" (expression slot) "]"))))

(define-javascript-special-operator ref :expression (processor expr &rest slots)
  (expression expr)
  (loop for slot in slots do
       (out "[" (expression slot) "]")))

(define-javascript-special-operator new :expression (processor expr &rest args)
  "new " (expression expr) 
  (when args
    (out "(" 
	 (loop for (e . rest) on args 
	    do (expression e)
	    when rest do (out ", "))
	 ")")))

(define-javascript-special-operator ++ :expression (processor lvalue &optional post)
  (if (eql post :post)
      (out (expression lvalue) "++")
      (out "++" (expression lvalue))))

(define-javascript-special-operator -- :expression (processor lvalue &optional post)
  (if (eql post :post)
      (out (expression lvalue) "--")
      (out "--" (expression lvalue))))


(define-unary-ops delete void typeof ~ !)

;; In theory, if we changed the PROGN to another macro, we could keep
;; track of precedence levels an avoid over parenthesizing in the
;; generated code. Though it's not clear that's actually a good idea.
(progn
  (define-binary-ops * / %)
  (define-binary-ops + -)
  (define-binary-ops << >> >>>)
  (define-binary-ops < > <= >= instanceof in)
  (define-binary-ops == != === !===)
  (define-binary-ops &)
  (define-binary-ops ^)
  (define-binary-ops \|) ;; hmmm. This may not be the best name. Unless we put the reader into a special mode.
  (define-binary-ops &&)
  (define-binary-ops \|\|)
  (define-assignment-ops = *= /= %= += -= <<= >>= >>>= &= ^= \|=))

(define-javascript-special-operator ? :expression (processor condition then &optional (else 'null))
  (process-javascript processor condition :expression)
  (raw-string processor " ? ")
  (process-javascript processor then :expression)
  (raw-string processor " : ")
  (process-javascript processor else :expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements -- special operators that produce statements

(define-javascript-special-operator progn :expression (processor &rest body)
  "(" 
  (loop for (e . rest) on body do (out (expression e)) (when rest (out ", ")))
  ")")

(define-javascript-special-operator prog :statement (processor &rest body)
  (loop for s in body do (out (statement s) :freshline)))

;; Block -- we handle this a bit specially to avoid creating redundant
;; blocks in the generated code. In the case where a block contains
;; only another block (or a macro that expands into a block) we strip
;; the inner block.
(define-javascript-special-operator block :statement (processor &rest body)
  (when (and body (not (cdr body)))
    (loop while (javascript-macro-form-p (car body)) do
	 (setf (car body) (expand-javascript-macro-form (car body))))
    (loop while (and body (not (cdr body)) (consp (car body)) (eql (caar body) 'block)) do
	 (setf body (rest (car body)))))

  (out 
   "{" :newline :indent
   (loop for stmt in body do (out (statement stmt) :freshline))
   :unindent
   "}"))

;; Var -- can only define one variable at a time.
(define-javascript-special-operator var :statement (processor variable &optional value)
  :freshline
  "var " (emit (symbol-name variable))
  (when value (out " = " (expression value))) ";")

;; If
(define-javascript-special-operator if :statement (processor condition then &optional else)
  "if (" (expression condition) ") " (statement then)
  (when else (out " else " (statement else))))

;; Do-While 
(define-javascript-special-operator do-while :statement (processor body condition)
  "do " (statement body) " while (" (expression condition) ");")

;; While 
(define-javascript-special-operator while :statement (processor condition body)
  "while (" (expression condition) ") " (statement body))

;; For
(define-javascript-special-operator for :statement (processor var-test-step statement)
  (destructuring-bind (var test &optional step) var-test-step
    (let ((var-p (and (consp var) (eql (car var) 'var))))
      (if var-p (setf var (cadr var)))
      (if (eql test 'in)
	  (out "for (" (if var-p (out "var ")) (expression var) " in " (expression step) ") " (statement statement))
	  (out "for (" (if var-p (out "var ")) (statement var) " " (statement test) " " (expression step) ") " (statement statement))))))

;; Continue
(define-javascript-special-operator continue :statement (processor &optional id)
  "continue" (when id (out " " (emit (symbol-name id)))) ";")

;; Break
(define-javascript-special-operator break :statement (processor &optional id)
  "break" (when id (out " " (emit (symbol-name id)))) ";")

;; Return
(define-javascript-special-operator return :statement (processor &optional expr)
  "return" (when expr (out " " (expression expr))) ";")

;; With
(define-javascript-special-operator with :statement (processor expr stmt)
  "with (" (expression expr) ") " (statement stmt))

;; Switch
(define-javascript-special-operator switch :statement (processor expr &rest clauses)
  "switch (" (expression expr) ") {" :newline :indent
  (loop for (e . statements) in clauses do
       (if (eql e :default)
	   (out "default:" :newline :indent)
	   (out "case " (expression e) ":" :newline :indent))
       (loop for s in statements do (statement s) (out :freshline))
     (out :freshline :unindent))
  :freshline :unindent
  "}")

;; Labeled statement
(define-javascript-special-operator label :statement (processor label statement)
  (emit (symbol-name label)) ": " (statement statement))

;; Throw
(define-javascript-special-operator throw :statement (processor expr)
  "throw " (expression expr) ";")

;; Try
(define-javascript-special-operator try :statement (processor &rest body)
  (flet ((key (e) (if (consp e) (first e))))
    (let ((catch-clause (find 'catch body :key #'key))
	  (finally-clause (find 'finally body :key #'key)))
      (when catch-clause
	(assert 
	 (let ((next (cdr (member catch-clause body))))
	   (or (null next) (eql (car next) finally-clause)))))
      (when finally-clause
	(assert (null (cdr (member finally-clause body)))))
      
      (setf body (ldiff body (or (member catch-clause body)
				 (member finally-clause body))))
      (out
       "try {" :newline :indent
       (loop for stmt in body do (out (statement stmt) :freshline))
       :unindent :freshline "}"
       (when catch-clause
	 (destructuring-bind (var &rest body) (rest catch-clause)
	   (out 
	    " catch (" (name var) ") {" :newline :indent
	    (loop for stmt in body do (out (statement stmt) :freshline))
	    :unindent :freshline "}")))
       (when finally-clause
	 (out 
	  " finally {" :newline :indent
	    (loop for stmt in (rest finally-clause) do (out (statement stmt) :freshline))
	    :unindent :freshline "}"))))))

;; Function -- two kinds, named and anonymous. The former is a
;; statement; the latter an expression.
(define-javascript-special-operator function :statement (processor &rest body)
  (flet ((params (params)
	   (out "("
		(loop for (p . rest) on params do
		     (out (name p)) (when rest (out ", ")))
		")"))
	 (body (body)
	   (process-javascript processor `(block ,@body) :statement)))
    (if (and (symbolp (first body)) (not (null (first body))))
	(destructuring-bind (name (&rest params) &rest body) body
	  (out "function " (name name) " " (params params) " " (body body)))
	(destructuring-bind ((&rest params) &rest body) body
	  (out "function " (params params) " " (body body))))))
  