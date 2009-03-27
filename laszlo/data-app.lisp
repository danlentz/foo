(in-package :laszlo.examples)

;;; db stuff

(defparameter *root-path*
  "C:/Documents and Settings/manuel/My Documents/local-svn/laszlo/")

(defparameter *contact-dtd*
  (cxml:parse-dtd-file (merge-pathnames "contact.dtd" *root-path*)))

(defclass contact ()
  ((first-name :initarg :first-name :accessor first-name
	       :attribute "firstName")
   (last-name :initarg :last-name :accessor last-name
	      :attribute "lastName")
   (phone :initarg :phone :accessor phone
	  :attribute "phone")
   (email :initarg :email :accessor email
	  :index-type unique-index
	  :index-initargs (:test #'equal)
	  :index-reader contact-with-email
	  :index-values all-contacts
	  :attribute "email"))
  (:unique-id-slot email)
  (:unique-id-reader #'contact-with-email)
  (:metaclass xml-class)
  (:dtd *contact-dtd*)
  (:element "contact"))

(defmethod print-object ((c contact) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (first-name last-name email) c
      (format stream "\"~A ~A\" (~A)" first-name last-name email))))

#+nil
(make-instance 'contact
	       :first-name "Manuel"
	       :last-name "Odendahl"
	       :phone "2394802934"
	       :email "manuel@bl0rg.net")

(net.aserve:publish :path "/phonebook"
		    :content-type "text/xml"
		    :function #'(lambda (req ent)
				  (xml-to-http (req ent)
					       (all-contacts) :name "phonebook")))

(defun phonebook-update (req)
  (with-query-params (req action xml pk)
    (warn "action ~A, xml ~A" action xml)
    (cond
      ((string= action "insert")
       (insert-xml-class 'contact xml :parse #'bknr.impex:parse-xml-stream))
      ((string= action "update")
       (update-xml-class 'contact xml :parse #'bknr.impex:parse-xml-update-stream))
      ((string= action "delete")
       (destroy-object (contact-with-email pk))))))

(net.aserve:publish :path "/phonebook-update"
		    :content-type "text/xml"
		    :function #'(lambda (req ent)
				  (handler-case (phonebook-update req)
				    (error (e)
				      (warn "error: ~A" e)
				      (xml-http-output (req ent)
						       (:result "failure")))
				    (:no-error (e)
				      (declare (ignore e))
				      (xml-http-output (req ent)
						       (:result "success"))))))


(net.aserve:start :port 8081)


(laszlo-file "beispiel.lzx"
  ((:canvas :width 800)
   (:window
    ((:method :name "jsmethod" :args "bla,blub")
     (xml ret (:update (:user :name (first-name.get-text))))
     (return ret)))))
      
(laszlo-file "data-app8.lzx"
  ((:canvas :bgcolor "#D4D0C8" :debug "true")
   (:include :href "xml-lib.lzx")
   (:dataset :name "dset" :src "http://localhost:8081/phonebook" :request "true" :type "http")

   ((:dataset :name "dsSendData" :request "false" :src "http://localhost:8081/phonebook-update"
	      :type "http"))

   ((:datapointer :xpath "dsSendData:/")
    ((:method :event "ondata")
     (if (= (this.xpath-query "result/text()") "success")
	 (-Debug.write "Operation succeeded")
	 (-Debug.write "Operation failed"))))

   ((:class :name "contactview" :extends "view" :visible "false" :x 20 :height 120)
    (:text :name "pk" :visible "false" :datapath "@email")
    (:text :y 10 "First Name:")
    (:edittext :name "firstName" :datapath "@firstName" :x 80 :y 10)
    (:text :y 35 "Last Name:")
    (:edittext :name "lastName" :datapath "@lastName" :x 80 :y 35)
    (:text :y 60 "Phone:")
    (:edittext :name "phone" :datapath "@phone" :x 80 :y 60)
    (:text :y 85 "Email:")
    (:edittext :name "email" :datapath "@email" :x 80 :y 85)

    ((:method :name "sendData" :args "action")
     (let ((d canvas.datasets.ds-send-data)
	   (p (new -lz-param)))
       (xml xmlret (:update (:contact :first-name (first-name.get-text)
				      :last-name (last-name.get-text)
				      :phone (phone.get-text)
				      :email (email.get-text))))
       (p.add-value "action" action t)
       (p.add-value "xml" (xmlret.serialize) t)
       (p.add-value "pk" (pk.get-text) t)
       (d.set-query-string p)
       (d.do-request))))

   (:simplelayout :axis "y")

   (:view
    (:simplelayout :axis "y")
    (:text :onclick (parent.new-contact.set-visible (not parent.new-contact.visible))
	   "New Entry...")

    ((:contactview :name "newContact" :datapath "new:/contact")
     ((:button :width 80 :x 200 :y 10) "Add"
      ((:method :event "onclick")
       (parent.send-data "insert")
       (parent.datapath.update-data)
       (let ((dp (canvas.datasets.dset.get-pointer)))
	 (dp.select-child)
	 (dp.add-node-from-pointer parent.datapath)
	 (parent.set-visible false)
	 (parent.set-datapath "new:/contact")))))
    
    ((:view :datapath "dset:/phonebook/contact")
     (:simplelayout :axis "y")
     ((:view :name "list" :onclick (parent.update-contact.set-visible
				    (not parent.update-contact.visible)))
      (:simplelayout :axis "x")
      (:text :datapath "@firstName")
      (:text :datapath "@lastName")
      (:text :datapath "@phone")
      (:text :datapath "@email"))

     ((:contactview :name "updateContact")
      ((:button :width 80 :x 200 :y 10) "Update"
       ((:method :event "onclick")
	(parent.send-data "update")
	(parent.parent.datapath.update-data)))

      ((:button :width 80 :x 200 :y 40) "Delete"
       ((:method :event "onclick")
	(parent.send-data "delete")
	(parent.parent.datapath.delete-node))))))
   
    (:debug :width 600 :height 120)))
          