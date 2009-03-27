(in-package :cl-user)

(defun guitar-neck (frets)
  (let ((len (* (1+ frets) 50)))
    (xml:xml
      (:line :class "endneck" :x1 92 :y1 87 :x2 292 :y2 87)
      (:rect :class "neck" :x 100 :y 100 :width 184 :height (:print len))
      (dotimes (i frets)
	(xml:xml
	  (:line :class "frets"
		 :x1 102 :y1 (:print (+ (* i 50) 152))
		 :x2 282 :y2 (:print (+ (* i 50) 152)))))
      (:line :class "strings" :x1 117 :y1 94 :x2 117 :y2 (:print (+ 94 len)))
      (:line :class "strings" :x1 147 :y1 94 :x2 147 :y2 (:print (+ 94 len)))
      (:line :class "strings" :x1 177 :y1 94 :x2 177 :y2 (:print (+ 94 len)))
      (:line :class "strings" :x1 207 :y1 94 :x2 207 :y2 (:print (+ 94 len)))
      (:line :class "strings" :x1 237 :y1 94 :x2 237 :y2 (:print (+ 94 len)))
      (:line :class "strings" :x1 267 :y1 94 :x2 267 :y2 (:print (+ 94 len)))

      (:path :class "spacer"
	     :d (:format
		 "M87,~A A100,40 0 0,0 192,~A A100,40 0 0,1 297,~A L297,~A L87,~A Z"
		 (+ len 72) (+ len 72) (+ len 72)
		 (+ len 150) (+ len 150)))
      
      (:path :class "spacerline"
	     :d (:format "M87,~A A100,40 0 0,0 192,~A A100,40 0 0,1 297,~A"
			 (+ len 72) (+ len 72) (+ len 72))))))

(defun dot (string fret)
  (if (= fret 0)
      (xml:xml
	(:circle :class "emptydot"
		 :r 10
		 :cx (:print (+ 87 (* 30 string)))
		 :cy (:print 63)))
      (xml:xml
	(:circle :class "dot"
		 :r 14
		 :cx (:print (+ 87 (* 30 string)))
		 :cy (:print (+ 127 (* 50 (1- fret))))))))

(defun dot-note (string note)
  (dot string (- note (elt *string-starts* (1- string)))))

(defun gen-chord (frets scale)
  (xml:xml
     (:xml-proc :xml :version "1.0" :encoding "iso-8859-1")
     (:xml-proc :xml--stylesheet :href "xchords-default.css" :type "text/css")
     ((:svg :xmlns "http://www.w3.org/2000/svg"
	    :width 280
	    :height (:print (+ 220 (* 50 (1+ frets))))
	    :view-box (:format "0 0 400 ~A" (+ 0 (* 100 (1+ frets)))) :fill "white")
      (:title "Chord C")
      ((:g :id "C")
       (guitar-neck 24)

       (dolist (pos (string-pos 0 '(0 3 5 7 9) frets))
	 (unless (= pos 0)
	   (xml:xml
	     ((:text :class "barrenum" :x 85 :y (:print (+ (* 50 pos) 90)))
	      (:format "~A." pos)))))

       #+nil
       (loop for string from 1 to 6
	    for fret-pos = (string-pos (elt *string-starts* (1- string)) scale frets)
	    do (dolist (fret fret-pos)
		 (dot string fret)))

       (let* ((scale (scale-start-from (scale-on-guitar *aeolian*) 48))
	      (strings (group-by scale 4)))
	 (warn "strings ~A" strings)
	 (loop for string in strings
	      for i from 1 to 6
	      do (dolist (note string)
		   (dot-note i note))))
       
       #|
       (:circle :class "dot" :cx 147 :cy 227 :r 14)
       ((:text :class "finger" :x 141 :y 235) "3")
       (:circle :class "dot" :cx 177 :cy 177 :r 14)
       ((:text :class "finger" :x 171 :y 185) "2")
       (:circle :class "dot" :cx 237 :cy 127 :r 14)
       ((:text :class "finger" :x 230 :y 135) "1")
       |#

       ))))

(defun group-by (list num)
  (loop for group on list by #'(lambda (seq) (subseq seq num))
	collect (subseq group 0 (min (length group) num))))

(defun scale-start-from (scale start)
  (remove-if #'(lambda (x) (< x start)) scale))

(defun scale-on-guitar (scale)
  (let ((from (scale-from 40 scale)))
    (string-pos 40 from 48)))

(defun scale-from (note scale)
  (let ((start (mod note 12)))
    (sort (mapcar #'(lambda (i) (mod (- i start) 12)) scale) #'<)))

(defun string-pos (start pos max)
  (remove-if #'(lambda (x) (> x (+ start max)))
	     (loop for i from 0 below max by 12
		appending (mapcar #'(lambda (x) (+ i x start)) pos))))

(defun list-range (list)
  (- (reduce #'max list :initial-value most-negative-fixnum)
     (reduce #'min list :initial-value most-positive-fixnum)))

(defun group-by-range (list range)
  (do (res
       group
       (l list (cdr l)))
      ((null l)
       (push (nreverse group) res)
       (nreverse res))
    (let ((elt (car l)))
      (if (<= (list-range (cons elt group)) range)
	  (push elt group)
	  (progn
	    (push (nreverse group) res)
	    (setf group (list elt)))))))

(defun map-list-to-strings (list)
  (if (> (length list) 6)
      (subseq list 0 6)
      list))

(defun lowest-note (list)
  (reduce #'min list :initial-value most-positive-fixnum))

(defun lowest-fret (list string)
  (note-fret (lowest-note list) string))

(defun highest-note (list)
  (reduce #'max list :initial-value most-negative-fixnum))

(defun highest-fret (list string)
  (note-fret (highest-note list) string))

(defun note-fret (note string)
  (- note (string-start-note string)))

(defun fret-range (list string)
  (list (note-fret (lowest-note list) string)
	(note-fret (highest-note list) string)))

;;; semitones from C
;; C - 0
;; Db - 1
;; D  - 2, Eb - 3, E - 4, F - 5, Gb - 6, G - 7, Ab - 8, A - 9, Bb - 10, B - 11

(defun string-start-note (string)
  (elt *string-starts* (1- string)))
   
(defparameter *string-starts* '(40 ; E
				45 ; A
				50  ; D
				55  ; G
				59 ; B
				64 ; E
				))

(defvar *ionian*  '(0 2 4 5 7 9 11))
(defvar *aeolian* '(0 2 3 5 7 8 10))

(defun parse-scale (req ent)
  (net.aserve:with-http-response (req ent)
    (net.aserve:with-http-body (req ent)
      (xml:with-xml-output (net.html.generator:*html-stream* :pretty t)
	(gen-chord 18 *ionian*)))))

(net.aserve:publish :path "/chord"
		    :content-type "application/xml"
		    :function #'parse-scale)

(net.aserve:publish-file :file "xchords-default.css" :path "/xchords-default.css")