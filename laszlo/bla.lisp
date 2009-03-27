(use-package :xml)
(use-package :js)

(defmacro laszlo-file (filename &body body)
  `(with-xml-to-file (,filename)

;; (format nil
;;     "C:/Program Files/OpenLaszlo Server 3.1.1/Server/lps-3.1.1/my-apps/~a"
;;     filename))

     (xml ,@body)))

(defun generate-data-lzx ()
  (laszlo-file "lisp1.lzx"
    ((:canvas :height 180 :width 500 :debug "true")
     ((:dataset :name "myData" :src "myShowData.xml"))
     ((:datapointer :xpath "myData:/" :ondata (:js (process-data)))
      ((:method :name "processData")
       (this.select-child 2)
       (do () (this.select-next)
	 (if (= (this.xpath-query "@show") "south park")
	     (-Debug.write (this.xpath-query "firstName/text()")))))))))

(laszlo-file "lisp2.lzx"
  ((:canvas :width "100%" :height 200)

   ((:view :name "remotedata" :layout "axis:y" :bgcolor "#cccccc")
    (:method :event "onclick"
	     (-debug.write(this.lds)))
    
    (:dataset :name "lds" :src "http://127.0.0.1:8080/data" :type "http" :request "true")
    
    (:text "local dataset loaded at runtime")
    (:text :datapath "local:parent.lds:/persons/person/firstName/text()"
	   :onclick (-Debug.write this.datapath))
    
    ((:method :reference "lds" :event "ondata")
     (-Debug.write "remotedata test data loaded" this)
     (unless (= (this.lds.serialize) "bla")
       (-Debug.error "remotedata serialized data does not match expected value"))))))
