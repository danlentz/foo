(in-package :js)

;;;# Compiler macros
;;;t \index{compiler macro}

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *js-compiler-macros* (make-hash-table)
    "*JS-COMPILER-MACROS* is a hash-table containing the
functions corresponding to javascript special forms, indexed by
their name. Javascript special forms are compiler macros for JS
expressions."))

(defmacro define-js-compiler-macro (name lambda-list &rest body)
  "Define a javascript compiler macro NAME. Arguments are destructured
according to LAMBDA-LIST. The resulting JS language types are appended
to the ongoing javascript compilation."
  (let ((js-name (klammer-intern
                  (concatenate 'string "JS-" (symbol-name name)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defun ,js-name ,lambda-list ,@body)
      (setf (gethash ',(klammer-intern name) *js-compiler-macros*)
            #',js-name))))

(defun js-get-compiler-macro (name)
  (when (symbolp name)
    (gethash (klammer-intern name) *js-compiler-macros*)))

(defun js-compiler-macro-form-p (form)
  (when (js-get-compiler-macro form)
    t))

;;;# KlammerScript macro expansion
;;;t \index{macros}
;;;t \index{KlammerScript macros}
;;;t \index{macroexpansion}

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *js-macro-toplevel* (make-hash-table)
    "Toplevel of macro expansion, holds all the toplevel javascript macros.")
  (defvar *js-macro-env* (list *js-macro-toplevel*)
    "Current macro environment."))

(defun lookup-macro (name)
  "Lookup the macro NAME in the current macro expansion
environment. Returns the macro and the parent macro environment of
this macro."
  (do ((env *js-macro-env* (cdr env)))
      ((null env) nil)
    (let ((val (or (gethash name (car env))
                   ;; default : check in klammerscript package
                   (gethash (klammer-intern name) (car env)))))
      (when val
	(return-from lookup-macro
	  (values val (or (cdr env)
			  (list *js-macro-toplevel*))))))))

(defmacro defjsmacro (name args &rest body)
  "Define a javascript macro, and store it in the toplevel macro environment."
  (when (js-get-compiler-macro name)
    (warn "Redefining compiler macro ~S" name)
    (remhash (klammer-intern name) *js-compiler-macros*))
  (let ((lambda-list (gensym)))
    `(setf (gethash ',name *js-macro-toplevel*)
      #'(lambda (&rest ,lambda-list)
	  (destructuring-bind ,args ,lambda-list ,@body)))))
  
(defun js-expand-form (expr)
  "Expand a javascript form."
  (cond ((and (atom expr)
              (not (symbolp expr)))
         expr)
        ((symbolp expr)
	 (multiple-value-bind (js-macro macro-env)
	     (lookup-macro expr)
	   (if js-macro
	       (js-expand-form (let ((*js-macro-env* macro-env))
				 (funcall js-macro)))
	       expr)))
	
	((js-compiler-macro-form-p expr) expr)
	
	((equal (first expr) 'quote) expr)

	(t (let ((js-macro (lookup-macro (car expr))))
	     (if js-macro
		 (js-expand-form (apply js-macro (cdr expr)))
		 expr)))))

(defvar *var-counter* 0)

(defun js-gensym (&optional (name "js"))
  (klammer-intern (format nil "tmp-~A-~A" name (incf *var-counter*))))

