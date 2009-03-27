;;;;; -*- Mode: lisp; coding: iso-8859-1; -*-
;;;;;
;;;;; Enhanced Markdown text markup system to Common Lisp
;;;;;     Dan Lentz <danlentz@gmail.com>
;;;;;
;;;;;     based on markdown.lisp by
;;;;;      Levi Pearson <levi@cold.org>



(in-package :foo.markdown)

(defparameter *html-placeholder* "qaodmasdkwaspemas~Sajkqlsmdqpakldnzsdfls")

;; Global vars

(defparameter *inline-matchers* nil)
(defparameter *preprocessors* nil)
(defvar *html-store* nil)
(defvar *html-store-counter* 0)
(defvar *reference-store* nil)

(defparameter *matcherlist* '(double-backtick backtick escape image-link
			       image-reference reference link-angled link
			       autolink automail html entity not-strong
			       strong-em strong-em-2 strong strong-2
			       emphasis emphasis-2))

;; Utility functions

(defun make-inline-scanner (pattern)
  (create-scanner
   `(:sequence
     :start-anchor
     (:register (:greedy-repetition 0 nil :everything))
     ,pattern
     (:register (:greedy-repetition 0 nil :everything))
     :end-anchor)
   :single-line-mode t))

(defun make-beginline-scanner (pattern)
  (create-scanner
   `(:sequence
     (:register (:greedy-repetition 0 nil :everything))
     ,pattern
     (:register (:greedy-repetition 0 nil :everything))
     :end-anchor)
   :single-line-mode t))

(defun dequote (str)
  "Remove surrounding quotes from a string"
  (if (and (eq (elt str 0) #\")
	   (eq (elt str (1- (length str))) #\"))
      (subseq str 1 (- (length str) 1))
      str))

(declaim (inline strip))
(defun strip (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(declaim (inline strip-nl))
(defun strip-nl (str)
  (string-trim '(#\Newline) str))

(defun strip-left (str)
  (string-left-trim '(#\Space #\Tab #\Newline) str))

(defun strip-right (str)
  (string-right-trim '(#\Space #\Tab #\Newline) str))

(defun starts-with (line string)
  (let ((linelen (length line))
	(strlen (length string)))
    (and (>= linelen strlen)
	 (string= string (subseq line 0 strlen)))))

(defun ends-with (line string)
  (let ((linelen (length line))
	(strlen (length string)))
    (and (>= linelen strlen)
	 (string= string (subseq line (- linelen strlen) linelen)))))

(defun detab (str)
  "Remove leading tab or 4 spaces"
  (cond ((string= str "")
	 str)
	((starts-with str "    ")
	 (subseq str 4))
	((eq (elt str 0) #\Tab)
	 (subseq str 1))
	(t str)))

(defun entity-escape (str)
  "Replace & with &amp;, < with &lt;, > with &gt;"
  (let ((result str))
    (setf result (regex-replace-all "&" result "&amp;"))
    (setf result (regex-replace-all "<" result "&lt;"))
    (setf result (regex-replace-all ">" result "&gt;"))
    result))

(defun quote-escape (str)
  "Replace \" with &quot;"
  (regex-replace-all "\"" str "&quot;"))

(defun split-on-eol (text)
  (split "(\\n)|(\\r\\n)|(\\r)" text))

(defun split-on-double-eol (text)
  (split "((\\n)|(\\r\\n)|(\\r)){2}" text))

(defun join-list-of-strings (list-of-strings &optional (newlines 1))
  (let ((control (if (= newlines 1)
		     "~{~A~%~}"
		     "~{~A~%~%~}")))
    (format nil control list-of-strings)))

(defun def-inline-matcher (name pattern match-fun)
  (push (cons name (cons pattern match-fun)) *inline-matchers*))

(defun handle-inline (string)
  (let ((strlen (length string)))
    (cond ((= strlen 0)
	   (list " ") nil)
	  ((and (> strlen 2)
		(eq (elt string (1- strlen)) #\Space)
		(eq (elt string (- strlen 2)) #\Space))
	   `(,@(handle-inline (subseq string 0 (- strlen 2))) (:br)))
	  
	  ((dolist (matcher *matcherlist*)
	     (let ((result (match-inline matcher string)))
	       (when result (return result)))))
	  (t
	   (list (replace-placeholder (entity-escape string)))))))

(defun match-inline (matcher string)
  (let ((matchrec (cdr (assoc matcher *inline-matchers*))))
    (multiple-value-bind (matched match)
	(scan-to-strings (car matchrec) string)
      (when matched
	(let ((obj (funcall (cdr matchrec) match))
	      (pred (handle-inline (elt match 0)))
	      (succ (handle-inline (elt match (1- (length match))))))
	  (if (and (stringp (car pred))
		   (stringp obj)
		   (stringp (car succ)))
	      (list (concatenate 'string
				 (car pred)
				 (replace-placeholder obj)
				 (car succ)))
	      `(,@pred
		  ,obj
		  ,@succ)))))))

(defun store-html (str)
  "Got a string of HTML, store it safely away and return a keyed placeholder"
  (let ((placehold (format nil *html-placeholder* *html-store-counter*)))
    (incf *html-store-counter*)
    (setf *html-store* (reverse (cons str (reverse *html-store*))))
    placehold))

(defun retrieve-html (idx)
  "Retrieve the HTML associated with the key from the store"
  (elt *html-store* idx))

(defun store-reference (id href)
  (push (cons (intern (string-upcase id)) href) *reference-store*))

(defun lookup-reference (id)
  "Retrieve the href associated with the id"
  (let ((ref (cdr (assoc id *reference-store*))))
    (if (consp ref)
	(values (car ref) (cdr ref))
	ref)))

(defun index-from-placeholder (str)
  (multiple-value-bind (matchp strlst)
      (scan-to-strings '(:sequence
			 "qaodmasdkwaspemas"
			 (:register (:greedy-repetition 0 nil :digit-class)))
		       str)
    (when matchp (parse-integer (elt strlst 0)))))

(defun replace-placeholder (str)
  (multiple-value-bind (matchp strlist)
      (scan-to-strings '(:sequence
			 (:register (:greedy-repetition 0 nil :everything))
			 "qaodmasdkwaspemas"
			 (:register (:greedy-repetition 0 nil :digit-class))
			 "ajkqlsmdqpakldnzsdfls"
			 (:register (:greedy-repetition 0 nil :everything)))
		       str)
    (if matchp
	(concatenate 'string
		     (elt strlist 0)
		     (retrieve-html (parse-integer (elt strlist 1)))
		     (elt strlist 2))
	str)))
	
;; Regular expression that matches non-bracket characters
(define-parse-tree-synonym nobracket+
    (:greedy-repetition 1 nil (:inverted-char-class #\[ #\])))

(define-parse-tree-synonym nobracket
    (:greedy-repetition 0 nil (:inverted-char-class #\[ #\])))

(define-parse-tree-synonym brackets
    (:SEQUENCE #\[ (:REGISTER nobracket+) #\]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inline matchers and associated functions

(defun simple-text-match (match)
  (elt match 1))

(def-inline-matcher 'escape
    (make-inline-scanner
     '(:sequence
       #\\
       (:register :everything)))
  #'simple-text-match)

(def-inline-matcher 'not-strong
    (make-inline-scanner
     '(:register " * "))
  #'simple-text-match)

(defun create-simple-tag-match (tag &optional (escape nil))
    (lambda (match)
      (let* ((raw-text (elt match 1))
	     (text (if escape
		       (entity-escape raw-text)
		       raw-text)))

	(list tag text))))

(def-inline-matcher 'backtick
    (make-inline-scanner
     '(:sequence
       #\`
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\`)))
       #\`))
  (create-simple-tag-match :code t))

(def-inline-matcher  'double-backtick
    (make-inline-scanner
     '(:sequence
       "``"
       (:register (:greedy-repetition 0 nil :everything))
       "``"))
  (create-simple-tag-match :code t))

(def-inline-matcher 'emphasis
    (make-inline-scanner
     '(:sequence
       #\*
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\*)))
       #\*))
  (create-simple-tag-match :em))

(def-inline-matcher 'emphasis-2
    (make-inline-scanner
     '(:sequence
       #\_
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\_)))
       #\_))
  (create-simple-tag-match :em))

(def-inline-matcher 'strong
    (make-inline-scanner
     '(:sequence
       "**"
       (:register (:greedy-repetition 0 nil :everything))
       "**"))
  (create-simple-tag-match :strong))

(def-inline-matcher 'strong-2
    (make-inline-scanner
     '(:sequence
       "__"
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\_)))
       "__"))
  (create-simple-tag-match :strong))

(defun create-double-tag-match (tag1 tag2)
  (lambda (match)
    (list tag1 (list tag2 (elt match 1)))))

(def-inline-matcher 'strong-em
    (make-inline-scanner
     '(:sequence
       "***"
       (:register (:greedy-repetition 0 nil :everything))
       "***"))
  (create-double-tag-match :strong :em))

(def-inline-matcher 'strong-em-2
    (make-inline-scanner
     '(:sequence
       "___"
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\_)))
       "___"))
  (create-double-tag-match :strong :em))

(defun link-match (match)
  (let* ((text (elt match 1))
	 (parts (elt match 2))
	 (partslist (split "\\s+" parts))
	 (href (if partslist
		   (first partslist)
		   ""))
	 (title (scan-to-strings "\".*\"" parts)))
    (if title
	(list :a :href href :title (entity-escape
				    (dequote title))
	      (entity-escape text))
	(list :a :href href (entity-escape text)))))

(def-inline-matcher 'link
    (make-inline-scanner
     '(:sequence
       brackets
       (:greedy-repetition 0 nil :whitespace-char-class)
       #\(
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\))))
       #\)))
  #'link-match)

(def-inline-matcher 'link-angled
    (make-inline-scanner
     '(:sequence
       brackets
       (:greedy-repetition 0 nil :whitespace-char-class)
       "(<"
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\))))
       ">)"))
  #'link-match)

(defun image-match (match)
   (list :img :src (entity-escape (elt match 2)) :alt (elt match 1)))

(def-inline-matcher 'image-link
    (make-inline-scanner    
     '(:sequence
       #\!
       brackets
       (:greedy-repetition 0 nil :whitespace-char-class)
       #\(
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\))))
       #\)))
  #'image-match)

(defun reference-match (match)
  (let* ((namestr (elt match 2))
	 (idstr (elt match 3))
	 (id (intern (string-upcase (if (plusp (length idstr)) idstr namestr)))))
    (multiple-value-bind (href title) (lookup-reference id)
      (if href
	  (if title
	      (list :a :href href :title title (entity-escape namestr))
	      (list :a :href href (entity-escape namestr)))
	  (elt match 1)))))

(def-inline-matcher 'reference
    (make-beginline-scanner
     '(:register
       (:sequence
	brackets
	(:greedy-repetition 0 nil :whitespace-char-class)
	#\[
	(:register (:greedy-repetition 0 nil (:inverted-char-class #\])))
	#\])))
  #'reference-match)

(defun image-reference-match (match)
  (let* ((namestr (elt match 1))
	 (idstr (elt match 2))
	 (id (intern (string-upcase (if (plusp (length idstr))
					idstr
					namestr)))))
    (multiple-value-bind (src title) (lookup-reference id)
      (if src
	  (if title
	      (list :img
		    :src (entity-escape src)
		    :title (entity-escape title)
		    :alt (entity-escape namestr))
	      (list :img
		    :src (entity-escape src)
		    :alt (entity-escape namestr)))
	  (concatenate 'string "BAD REFERENCE " namestr)))))

(def-inline-matcher 'image-reference
    (make-inline-scanner
     '(:sequence
       #\!
       brackets
       (:greedy-repetition 0 nil :whitespace-char-class)
       #\[
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\])))
       #\]))
  #'image-reference-match)

(defun autolink-match (match)
  (let ((href (elt match 1)))
    (list :a :href href (entity-escape href))))

(def-inline-matcher 'autolink
    (make-inline-scanner
     '(:sequence
       #\<
       (:register (:sequence
		   "http://"
		   (:greedy-repetition 0 nil (:inverted-char-class #\>))))
       #\>))
  #'autolink-match)

(defun automail-match (match)
  (let* ((addr (elt match 1))
	 (mailto (concatenate 'string "mailto:" addr)))
    (values (list :a :href mailto addr)
	    (elt match 0)
	    (elt match (1- (length match))))))

(def-inline-matcher 'automail
    (make-inline-scanner
    '(:sequence
      #\<
      (:register (:sequence
		  (:greedy-repetition 0 nil
				      (:inverted-char-class #\> #\Space))
		  #\@
		  (:greedy-repetition 0 nil
				      (:inverted-char-class #\> #\Space))))
      #\>))
  #'automail-match)

(defun html-match (match)
  (store-html (elt match 1)))

(def-inline-matcher 'html
    (make-inline-scanner    
    '(:register (:sequence
		 #\<
		 (:greedy-repetition 0 nil (:inverted-char-class #\>))
		 #\>)))
  #'html-match)

(def-inline-matcher 'entity
    (make-inline-scanner    
    '(:register (:sequence
		 #\&
		 (:greedy-repetition
		  0 nil
		  (:char-class #\#
			       (:range #\a #\z)
			       (:range #\A #\Z)
			       (:range #\0 #\9)))
		 #\;)))
  #'html-match)

;;;;;;;;;;;;;;;;;;;;;
;;; Preprocessors

(defun header-preprocessor (lines)
  (flet ((only-char-p (char line)
	   (dotimes (i (length line) i)
	     (unless (eq (elt line i) char)
	       (return nil)))))
    (let ((prevline "")
	  (outlines nil))
      (dolist (line lines outlines)
	(cond
	  ((plusp (length line))
	   (cond ((and (eq (elt line 0) #\=)
		       (only-char-p #\= line)
		       (not (string= (strip prevline) "")))
		  (pop outlines)
		  (push (concatenate 'string "# " prevline) outlines))
		 ((and (eq (elt line 0) #\-)
		       (only-char-p #\- line)
		       (not (string= (strip prevline) "")))
		  (pop outlines)
		  (push (concatenate 'string "## " prevline) outlines))
		 (t
		  (push line outlines)
		  (setf prevline line))))
	  (t
	   (push line outlines)
	   (setf prevline line))))
      (reverse outlines))))


(defun line-preprocessor (lines)
  (mapcar (lambda (line)
	    (cond
	      ((or (starts-with line "    ")
		   (starts-with line (coerce #(#\Tab) 'string)))
	       line)
	      ((scan "^\\s*([\\*_-]\\s?){3,}?$" line)
	       "<hr>")
	      (t
	       line)))
	  lines))


(defun html-block-preprocessor (lines)
  (let ((blocks (split-on-double-eol (join-list-of-strings lines))))
    (split-on-eol
     (join-list-of-strings
      (mapcar
       (lambda (blk)
	 (if (and (or (starts-with blk "<")
		      (starts-with blk (coerce #(#\Newline #\<) 'string)))
		  (ends-with
		   (string-right-trim '(#\Space #\Tab #\Newline) blk) ">"))
	     (store-html blk)
	     blk))
       blocks)
      2))))


(defun reference-preprocessor (lines)
  (let ((outlines nil))
    (dolist (line lines outlines)
      (multiple-value-bind (matchp matches)
	  (scan-to-strings "^\\ {0,3}\\[([^\\]]*)\\]:\\s*(.*)" line)
	(if matchp
	    (let ((id (string-downcase (strip (elt matches 0))))
		  (ref (dequote (elt (split "\\s" (elt matches 1)) 0)))
		  (title (scan-to-strings "\".*\"" (elt matches 1))))
	      (if title
		  (store-reference id (cons ref (dequote title)))
		  (store-reference id ref)))
	    (push line outlines))))
    (reverse outlines)))


;;;;;;;;;;;;;;;;;;;;;;;
;;; Block processors

(defun lines-until (lines condition)
  (let ((par nil)
	(rest nil)
	(switch nil))
    (dolist (line lines)
      (if switch
	  (push line rest)
	  (if (funcall condition line)
	      (progn (setf switch t)
		     (push line rest))
	      (push line par))))
    (values (reverse par) (reverse rest))))

(defun list-end (lines type)
  (let ((ol (create-scanner "^[ ]{0,3}\\d*\\.\\s+(.*)"))
	(ul (create-scanner "^[ ]{0,3}[*+-]\\s+(.*)"))
	(tab (create-scanner "^(?:\\t|(?:   ))(.*)"))
	(blanks 0)
	(inlist t)
	(itemno 0)
	(tight t)
	(outlist '())
	(rest '()))
    (dolist (line lines)
      (format t "processing line (~s, ~s, ~s): ~s~%" itemno inlist tight line)
      (if (string= (strip line) "")
	  (progn (incf blanks)
		 (when (not (> itemno 1))
		   (format t "no longer tight~%")
		   (setf tight nil))
		 (when (and tight (> itemno 1))
		   (setf inlist nil)))
	  (progn (when (or (> blanks 1)
			   (scan-to-strings (if (eq type :UL) ol ul) line)
			   (and (= blanks 1)
				(not (scan-to-strings
				      (if (eq type :UL) ul ol) line))
				(not (scan-to-strings tab line))))
		   (setf inlist nil))
		 (setf blanks 0)))
      (when (scan-to-strings (if (eq type :UL) ul ol) line)
	(format t "incrementing itemno~%")
	(incf itemno))
      (if inlist
	  (push line outlist)
	  (push line rest)))
    (format t "outlist: ~s~%rest: ~s~%" (reverse outlist) (reverse rest))
    (values (reverse outlist)
	    (reverse rest)
	    tight)))

(defun bq-end (lines)
  (lines-until lines (lambda (line)
		       (string= (strip line) ""))))

(defun tb-end (lines)
  (lines-until lines (lambda (line)
		       (not (or (string= (strip line) "")
				(starts-with line "    ")
				(starts-with line
					     (coerce #(#\Tab) 'string)))))))

(defun tabbed-block-processor (lines)
  `((:CODE
     (:PRE ,(string-right-trim
	     '(#\Newline)
	     (entity-escape (join-list-of-strings (mapcar #'detab lines))))))))

(defun blockquote-processor (lines)
  `((:BLOCKQUOTE ,@(section-processor
		    (mapcar (lambda (x) (if (> (length x) 2)
					    (subseq x 2)
					    ""))
			    lines)))))

(defun list-item-processor (lines tight)
  (let ((stripped (mapcar #'detab lines)))
    `(:LI ,@(section-processor stripped tight t))))

(defun list-processor (lines tag tight)
  (let ((ol (create-scanner "^[ ]{0,3}\\d*\\.\\s+(.*)"))
	(ul (create-scanner "^[ ]{0,3}[*+-]\\s+(.*)"))
	(item nil)
	(items nil)
	(text ""))
    (dolist (line lines)
      (cond ((or (and (eq tag :OL)
		      (multiple-value-bind (matchp matches)
			  (scan-to-strings ol line)
			(when matchp (setf text (elt matches 0)))
			matchp))
		 (and (eq tag :UL)
		      (multiple-value-bind (matchp matches)
			  (scan-to-strings ul line)
			(when matchp (setf text (elt matches 0)))
			matchp)))
	     (when item (push (reverse item) items))
	     (setf item nil)
	     (push text item))
	    ((string= (strip line) "")
	     (push line item))
	    (t
	     (push line item))))
    (when item
      (push (reverse item) items))
    `((,tag ,@(mapcar (lambda (x)
			(list-item-processor x tight))
		      (reverse items))))))

(defun section-processor (lines &optional (tightlist nil) (inlist nil))
  (let ((ul-regex (create-scanner "^[ ]{0,3}[*+-]\\s+(.*)"))
	(ol-regex (create-scanner "^[ ]{0,3}\\d*\\.\\s+(.*)")))
    (when lines
      (cond
					; HR tag
	((string= (strip (car lines)) "<hr>")
	 `((:HR) ,@(section-processor (cdr lines))))
					; unordered list
	((multiple-value-bind (matchp matches)
	     (scan-to-strings ul-regex (car lines))
	   (declare (ignore matches))
	   (when matchp
	     (multiple-value-bind (listlines rest tight) (list-end lines :UL)
	       `(,@(list-processor listlines :UL tight)
		 ,@(section-processor rest))))))

					; ordered list
	((multiple-value-bind (matchp matches)
	     (scan-to-strings ol-regex (car lines))
	   (declare (ignore matches))
	   (when matchp
	     (multiple-value-bind (listlines rest tight) (list-end lines :OL)
	       `(,@(list-processor listlines :OL tight)
		 ,@(section-processor rest))))))

					; block quote
	((multiple-value-bind (matchp matches)
	     (scan-to-strings "^> ?(.*)" (car lines))
	   (declare (ignore matches))
	   (when matchp
	     (multiple-value-bind (bqlines rest) (bq-end lines)
	       `(,@(blockquote-processor bqlines)
		 ,@(section-processor rest))))))

					; tabbed code block
	((multiple-value-bind (matchp matches)
	     (scan-to-strings "^(?:\\t|(?:   ))(.*)" (car lines))
	   (declare (ignore matches))
	   (when matchp
	     (multiple-value-bind (tblines rest) (tb-end lines)
	       `(,@(tabbed-block-processor tblines)
		 ,@(section-processor rest))))))

					; paragraph
	(t
	 (multiple-value-bind (paragraph rest)
	     (lines-until lines (lambda (line)
				  (or (not (> (length (strip line)) 0))
				      (if inlist
					  (or (scan-to-strings ul-regex line)
					      (scan-to-strings ol-regex line))
					  nil))))

	   (cond
					; check for header
	     ((and (> (length paragraph) 0)
		   (starts-with (car paragraph) "#"))
	      (multiple-value-bind (matchp matches)
		  (scan-to-strings "(#+)\\s([^#]*)(?:\\s#+)?"
				   (car paragraph))
		(if matchp
		    (let* ((level (length (elt matches 0)))
			   (tag (intern (format nil "H~s" level) :keyword))
			   (title (strip (elt matches 1))))
		      `((,tag ,title)
			,@(section-processor
			   (append (cdr paragraph) rest))))
		    "ERROR: Bad header")))

					; regular paragraph
	     ((> (length paragraph) 0)
	      (let ((out (handle-inline (join-list-of-strings paragraph))))
		(cond (tightlist
			`(,@out
			  ,@(section-processor rest)))
		      ((starts-with (car paragraph) "qaodmasdkwaspemas")
		       `(,@out
			 ,@(section-processor rest)))
		      ((and (= 1 (length out))
			    (string= (strip (first out)) ""))
		       (section-processor rest))
		      ((stringp (car (last out)))
		       `((:p ,@(butlast out) ,(strip-right (car (last out))))
			 ,@(section-processor rest)))
		      (t
		       `((:p ,@out)
			    ,@(section-processor rest))))))
					; plain text
	     (t
	      (section-processor
	       (cdr rest))))))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; External API 

(defun markdown-to-sexp (str)
  (setf *html-store* nil)
  (setf *html-store-counter* 0)
  (setf *reference-store* nil)
  (setf *preprocessors* (list #'html-block-preprocessor
			      #'header-preprocessor
			      #'line-preprocessor
			      #'reference-preprocessor))
  (let ((result (split-on-eol str)))
    (dolist (filter *preprocessors*)
      (setf result (funcall filter result)))
    (section-processor result)))

;;;;;;;;;;;;;;;;;
;;; Tests

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun run-test (filename)
  (let ((infile (concatenate 'string filename ".md"))
	(outfile (concatenate 'string filename ".html"))
	(html-sexp '()))
    (with-open-file (stream infile :direction :input)
      (let ((text (slurp-stream stream)))
;       (format t "Input: ~%---------~%~A~%---------~%" text)
;       (format t "Output:~%~{~S~%~}" (markdown-to-sexp text))
;       (print (markdown-to-sexp text))
	(setf html-sexp (cons :noescape (markdown-to-sexp text)))))
    (with-open-file (stream outfile :direction :output :if-exists :supersede)
      (let ((foo.html::*html-output* stream))
	(emit-html html-sexp)))))


(defun markdown-file (filename &optional (output-filename nil))
  (let ((infile (concatenate 'string filename ".md"))
	 (outfile (concatenate 'string
		    (or output-filename
		      (concatenate 'string filename ".html"))))
	(html-sexp '()))
    (with-open-file (stream infile :direction :input)
      (let ((text (slurp-stream stream)))
	(setf html-sexp (cons :noescape (markdown-to-sexp text)))))
    (with-open-file (stream outfile :direction :output :if-exists :supersede)
      (let ((foo.html::*html-output* stream))
	(emit-html html-sexp)))))

