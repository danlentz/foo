(in-package :cl-user)

(laszlo-file "userprofile.lzx"
  ((:canvas :bgcolor "#eaeaea" :width 700 :height 480)

   (:dataset :name "userInfo" :src "http://localhost:8081/userinfo" :request "false" :type "http")

   ((:class :name "editField" :extends "view" :x 10)
    (:attribute :name "description" :type "html")
    (:text :y (:constraint nil parent.y)
	   :x (:constraint nil parent.x)
	   :text (:constraint nil parent.description))
    (:edittext :name (:constraint nil parent.name)
	       :datapath (:constraint nil parent.datapath)
	       :x (:constraint nil (+ parent.x 150))
	       :y (:constraint nil parent.y)))
   
   ((:view :x 0 :y 0 :width "100%" :height "100%")
    (:simplelayout)
    
    ((:tabs :width (:constraint nil parent.width)
	    :height (:constraint nil parent.height))
     ((:tabpane :text "Preferences" :inset_left 10 :inset_top 10 :inset_right 10)
      (:text "foo"))

     ((:tabpane :text "User Information" :inset_left 10 :inset_top 10 :inset_right 10
		:datapath "new:/")

      (:text :x 10 :y 10 "SME Category:")
      (:button :x 150 :y 10 "Identify SME Category")
      (:edit-field :y 35 :description "Company Name:" :name "companyName" :datapath "companyName")
      (:edit-field :y 60 :description "VAT Number:" :name "vatNumber" :datapath "vatNumber"))

     ))))

(laszlo-file "bla.lzx"
  ((:canvas :height 400 :debug "true")

   (:dataset :name "userInfo" :src "http://localhost:8081/userinfo" :request "false" :type "http")
   
   ((:class :name "editField" :x 10)
    (:attribute :name "name")
    (:attribute :name "path" :type "string")
    (:attribute :name "description" :type "html")
    (:text :y (:constraint nil parent.y)
	   :x (:constraint nil parent.x)
	   :text (:constraint nil parent.description))
    (:edittext :name (:constraint nil parent.name)
	       :x (:constraint nil (+ parent.x 150))
	       :y (:constraint nil parent.y)
	       :datapath (:constraint nil parent.path)))

   ((:view :datapath "new:/userInfo[1]")
    (:simplelayout :axis "y")
    (:edit-field :y 10 :description "Bla" :name "bla" :path "@companyName")
    (:edit-field :y 25 :description "foobar" :name "foobar" :path "@vatNumber")
    ((:button :width 80 :x 40) "Update"
     ((:method :event "onclick")
      (let ((ds canvas.userInfo))
	(-debug.write (ds.serialize))))))

   (:debug)))
