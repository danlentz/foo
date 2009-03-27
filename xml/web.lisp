(in-package :laszlo)

(defmacro with-query-params ((request &rest params) &rest body)
  (let ((vars (loop for param in params
		    when (and (symbolp param)
			      (not (null param)))
		    collect (list param `(request-query-value ,(js:symbol-to-js param) ,request))
		    when (consp param)
		    collect (list (car param)
				  `(or (request-query-value ,(js:symbol-to-js (car param)) ,request)
				    ,(second param))))))
    (if vars
	`(let ,vars
	  ,@body)
	(first body))))

(defmacro xml-http-output ((req ent) &rest body)
  `(with-http-response (,req ,ent)
     (with-http-body (,req ,ent)
       (with-xml-output (net.html.generator:*html-stream* :pretty t)
	 (xml ,@body)))))


(defun output-xml-object (req ent class id)
  (let ((obj (bknr.datastore:store-object-with-id id)))
    (unless (and obj (typep obj (find-class class)))
      (xml-http-output (req ent)
        (:error "no object found")))
    (with-http-response (req ent)
      (with-http-body (req ent)
	(bknr.impex:write-to-xml
	 obj
	 :sink (cxml:make-octet-stream-sink net.html.generator:*html-stream*
					    :indentation 2 :canonical nil))))))

(defun output-xml-class (req ent class)
  (let ((objects (bknr.datastore:store-objects-with-class class)))
    (with-http-response (req ent)
      (with-http-body (req ent)
	(bknr.impex:write-to-xml
	 objects
	 :sink (cxml:make-octet-stream-sink net.html.generator:*html-stream*
					    :indentation 2 :canonical nil)
	 :name "objects")))))

(defun update-xml-class
  (class update
    ;; &key (parse #'bknr.datastore:parse-xml-persistent-update-stream) 
    )
  ( ;; excl:with-input-from-buffer
    with-input-from-string 
    ;;    (s (excl:string-to-octets update :null-terminate nil))
    (s (string-to-octets update :null-terminate nil))
    (funcall parse s (list (find-class class)))))

(defun insert-xml-class
  (class update
    ;; &key (parse #'bknr.datastore:parse-xml-persistent-stream)
    )
  ( ;;excl:with-input-from-buffer
    with-input-from-string
    ;; (s (excl:string-to-octets update :null-terminate nil))
    (s (string-to-octets update :null-terminate nil))
    (funcall parse s (list (find-class class)))))

(defun xml-handler (req ent class id)
  (with-query-params (req action update)
    (if (string-equal action "update")
	(handler-case 	(update-xml-class class update)
	  (error (e)
	    (warn "error: ~A" e)
	    (xml-http-output (req ent)
			     (:result "failure")))
	  (:no-error (e)
	    (declare (ignore e))
	    (xml-http-output (req ent)
			     (:result "success"))))
	(if id
	    (output-xml-object req ent class id)
	    (output-xml-class req ent class)))))

(defun publish-xml-handler (class)
  (let ((name (string-downcase (symbol-name class))))
    (net.aserve:publish-prefix
     :prefix (concatenate 'string "/" name)
     :content-type "text/xml"
     :function #'(lambda (req ent)
		   (let* ((path (subseq (net.uri:uri-path (net.aserve:request-uri req))
					(1+ (length name))))
			  (params (mapcar #'net.aserve:uridecode-string
					  (remove ""
						  (cl-ppcre:split "/" path)
						  :test #'equal)))
			  (id (when params (parse-integer (first params) :junk-allowed t))))
		     (xml-handler req ent class id))))))


(defmacro xml-to-http ((req ent) objs &key name)
  `(with-http-response (,req ,ent)
     (with-http-body (,req ,ent)
       (bknr.impex:write-to-xml ,objs :name ,name
				:sink (cxml:make-character-stream-sink
				       net.html.generator:*html-stream* :indentation 3
				       :canonical nil)))))
