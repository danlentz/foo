(in-package :cl-user)

(use-package :laszlo)

(laszlo-file "xml-lib.lzx"
  (:library
   (:script
    (defun make-simple-node (name attrs text)
      (return (new (-lz-data-element name attrs (list (new (-lz-data-text text))))))))))

(laszlo-file "tabs.lzx"
  ((:canvas :bgcolor "#EAEAEA" :width 800 :height 510)
   ((:view :x 20 :y 20)
    (:simplelayout)
    (:text "autosized")

    ((:tabs :x 5)
     ((:tabpane :inset_left 0 :inset_top 0 :inset_bottom 0 :inset_right 0) "insets 0"
      (:button "do nothing"))
     ((:tabpane :selected "true") "default"
      (:simplelayout :spacing 5)
      (:button "faster")
      (:radiogroup
       (:radiobutton "Curt")
       (:radiobutton "Prefer fairly wordy")
       (:radiobutton "I really prefer lots of extra words and fluff in my selections")))
     ((:tabpane :text "insets 10" :inset_left 10 :inset_top 10 :inset_bottom 10 :inset_right 10)
      (:button "don't press")))
    
    ((:view :height 10))
    (:text "set height and width")
    
    ((:tabs :x 5 :height 150 :width 300)
     ((:tabpane)
      "stuff"
      (:simplelayout :spacing 5)
      (:button "stuff")
      (:radiogroup
       (:radiobutton "test")
       (:radiobutton "other")))
     
     (:tabpane "more"
	       (:button "more"))))))

(laszlo-file "empty.lzx"
  (:canvas
   (:text "foobar")))

(laszlo-file "xml-showoff.lzx"
  (:canvas :debug :true
    (:text :width (:constraint (once) parent.width) "Hello"
      (:jsevent ondata ()
         (-debug.write "Hello"))
      (:jsmethod my-method (arg1 arg2 arg3)
         (return (+ arg1 arg2 arg3))))))

(laszlo-file "js-example.lzx"
  (:canvas
   (:view
    (:simplelayout :axis :y :spacing 15)
    (:text :onclick (parent.toggle-text.set-visible
		     (not parent.toggle-text.visible))
	   "Click here")
    (:text :name :toggle-text :bgcolor "#CDCDCD" :visible :false "Toggle text"))))


(laszlo-file "tree.lzx"
  ((:canvas :title "Tree Example" :bgcolor "#eaeaea" :width 830 :height 550)
   (:include :href "lz/tree.lzx")
   (:include :href "lz/slider.lzx")

   (:greenstyle :name "greencolors" :canvascolor "null")
   (:goldstyle :name "goldcolors" :canvascolor "null")

   (:class :name "mytree" :extends "tree" :expander "lztree_plusminus_rsc" :open "true")

   ((:dataset :name "mydata")
    ((:basket :name "basket" :isopen "true")
     ((:product :name "fruits" :isopen "true")
      ((:apples :name "aples" :isopen "true")
       (:type :name "red delicious apple" :isleaf "true"
	       :url "http://www.apple.com/")
       (:type :name "fuji apple" :isleaf "true"
	       :url "http://www.apple.com/")
       (:type :name "braeburn apple" :isleaf "true"
	       :url "http://www.apple.com/")
       (:type :name "honey crisp" :isleaf "true"
	       :url "http://www.apple.com/"))
      ((:oranges :name "oranges")
       (:type :name "valencia orange" :isleaf "true")
       (:type :name "blood orange" :isleaf "true")
       (:type :name "navel orange" :isleaf "true")))

     ((:product :name "dairy")
      ((:dairy :name "eggs")
       (:type :name "organic eggs" :isleaf "true")
       (:type :name "free-range eggs" :isleaf "true"))
      ((:dairy :name "milk")
       (:type :name "skim milk" :isleaf "true")
       (:type :name "1% milk" :isleaf "true")
       (:type :name "whole milk" :isleaf "true"))
      (:dairy :name "Yogurt"))))

   ((:view :x 20 :y 20)
    (:simplelayout :axis "x" :spacing 10)

    ((:view :width 250 :layout "spacing: 10")
     ((:view :width 210 :height 40 :bgcolor "black")
      ((:view :bgcolor "#eaeaea" :layout "spacing: 2" :x 2 :y 2
	      :width (:constraint nil (- parent.width 4))
	      :height (:constraint nil (- parent.height 4)))
       (:text "Double-click files for information")
       (:text "on fruits and dairy products")))

     ((:tree :style "goldcolors" :datapath "mydata:/basket"
	     :open "$path{'@isopen'}"
	     :text "$path{'@name'}"
	     :multiselect "true")
      ((:tree :id "xxx" :style "goldcolors" :datapath "*"
	      :text "$path{'@name'}" :isleaf "$path{'@isleaf'}")
       ((:method :event "onactivate")
	(when this.isleaf
	  (let ((url (this.datapath.xpath-query "@url")))
	    (if (!= url nil)
		(this.view-in-window url)
		(this.view-in-window (+ "http://www.google.com/search?q=" this.text))))))
       ((:method :name "viewInWindow" :args "url")
	(-lz-browser.load-u-r-l
	 (+ "javascript: var wptr = window.open('" url "','test','resizable=1,width=100,height=700,left=0,top=0,screenX=0,screenY=0,menubar,location,status,scrollbars,toolbar,address'); wptr.focus(); void(0);"))))))

    ((:view :width 320 :layout "spacing: 10")
     ((:view :width 210 :height 80 :bgcolor "black")
      ((:view :bgcolor "#eaeaea" :layout "spacing: 2" :x 2 :y 2
	      :width (:constraint nil (- parent.width 4))
	      :height (:constraint nil (- parent.height 4)))
       ((:text :align "center") "Hominid evolution")
       ((:text :multiline "true") "in millions of years before present")
       (:text "following Johanson & Edgar (1996)")
       ((:text :align "center")
	(:i "From Lucy to Language"))))
     (:mytree "Ardipithecus ramidus"
      (:mytree "Australopithecus"))))))

(laszlo-file "selection.lzx"
  (:canvas
   ((:dataset :name "fruits")
    (:fruit "Oranges")
    (:fruit "Apples")
    (:fruit "Bananas")
    (:fruit "Grapes")
    (:fruit "Kiwis")
    (:fruit "Papayas")
    (:fruit "Watermelon")
    (:fruit "Strawberries")
    (:fruit "Cherries"))

   (:simplelayout)

   (:text "Select a series of items below.")

   ((:view :name "fruitlist")
    (:selectionmanager :name "selector" :toggle "true")
    (:simplelayout)

    ((:view :name "listitem"
	    :datapath "fruits:/fruit"
	    :onclick (parent.selector.select this))

     (:text :name "txt" :datapath "text()")

     (:jsmethod set-selected (amselected)
       (let ((txt-color) (bgcolor))
	 (if amselected
	     (setf txt-color #xFFFFFF
		   bgcolor   #x999999)
	     (setf txt-color #x000000
		   bgcolor   #xFFFFFF))
	 (this.set-b-g-color bgcolor)
	 (this.txt.set-attribute "fgcolor" txt-color))))

    (:jsmethod delete-selected ()
      (let ((csel (this.selector.get-selection)))
	(dolist (i csel)
	  (.datapath.delete-node i))
	(this.selector.clear-selection))))

   ((:button :onclick (fruitlist.delete-selected)) "Delete Selection")))

(laszlo-file "dataselection.lzx"
  ((:canvas :width 200 :height 200)
   ((:dataset :name "mydata")
    (:list
     (:item "tricycle")
     (:item "train")
     (:item "racecar")
     (:item "scooter")
     (:item "bicycle")
     (:item "rollerblades")
     (:item "iceskates")
     (:item "minivan")
     (:item "sailboat")
     (:item "motorboat")))

   ((:class :name "selectme" :onclick (immediateparent.selector.select this)
	    :height 17 :width 100 :bgcolor "white")
    (:text :datapath "text()")
    (:jsmethod set-selected (isselected)
      (if isselected
	  (set-attribute "bgcolor" yellow)
	  (set-attribute "bgcolor" white))))

   ((:view :height 70 :clip "true")
    (:view
     (:dataselectionmanager :name "selector")
     (:selectme
      (:datapath :xpath "mydata:/list/item" :replication "lazy"))
     (:simplelayout))
    (:scrollbar))))

(laszlo-file "userdemo.lzx"
  ((:canvas :height 400 :debug "true")

   (:include :href "xml-lib.lzx")

   (:dataset :name "user" :src "http://127.0.0.1:8081/user/0" :type "http" :autorequest "true")

   ((:dataset :name "updateDs" :src "http://127.0.0.1:8081/user" :type "http"
	      :request "false" :autorequest "false"))

   ;;; wieso wird der scheiss nicht dann aufgerufen, wenn er aufgerufen werden soll? XXX
   ((:datapointer :xpath "updateDs:/")
    ((:method :event "ondata")
     (-debug.write (+ "on-data" (.serialize (this.get-dataset))))
     (-debug.write (this.xpath-query "/result[1]/text()"))
     (if (= (this.xpath-query "/result[1]/text()") "success")
	 (-debug.write "update was successful")
	 (-debug.write "update was not successful"))))
   
   (:datapointer :id "userdp" :xpath "user:/user[1]")

   (:simplelayout :axis "y")

   (:jsmethod edit-user ()
     (if this.edit-data.visible
	 (this.user.do-request)
	 (this.edit-data.set-data))
	 
     (this.show-data.set-visible (not this.show-data.visible))
     (this.edit-data.set-visible (not this.edit-data.visible)))

   ((:view :name "showData" :datapath "user:/user[1]" :visible "true")
    (:text :x 0 :y 0 "First Name:")
    (:text :x 150 :y 0 :name "firstName" :datapath "firstName/text()")
    (:text :x 0 :y 25 "Last Name:")
    (:text :x 150 :y 25 :name "lastName" :datapath "lastName/text()")
    (:text :x 0 :y 50 "VAT Number:")
    (:text :x 150 :y 50 :name "vatNumber" :datapath "vatNumber/text()")
    (:button :y 100 :onclick (parent.parent.edit-user) "Edit Data"))
   
   ((:view :name "editData" :visible "false")
    (:jsmethod set-data ()
       (this.first-name.set-text (userdp.xpath-query "/user[1]/firstName/text()"))
       (this.last-name.set-text  (userdp.xpath-query "/user[1]/lastName/text()"))
       (this.vat-number.set-text (userdp.xpath-query "/user[1]/vatNumber/text()")))

    (:jsmethod update-data ()
       (xml update (:update ((:user :id (userdp.xpath-query "/user[1]/@id"))
			     (:first-name (this.first-name.get-text))
			     (:last-name  (this.last-name.get-text))
			     (:vat-number (this.vat-number.get-text)))))
       (let ((p (new -lz-param)))
	 (p.add-value "action" "update" t)
	 (p.add-value "update" (update.serialize) t)
	 (update-ds.set-query-string p)
	 (-debug.write "update-ds.do-request")
	 (update-ds.do-request)
	 (this.parent.edit-user)))
    
    (:text :x 0 :y 0 "First Name:")
    (:edittext :x 150 :y 0 :name "firstName")
    (:text :x 0 :y 25 "Last Name:")
    (:edittext :x 150 :y 25 :name "lastName")
    (:text :x 0 :y 50 "VAT Number:")
    (:edittext :x 150 :y 50 :name "vatNumber")

    (:button :y 100 :onclick (parent.update-data) "Update Data"))

   ((:button :onclick (progn (parent.form.datapath.update-data)
			     (t.set-text (userprofile.serialize)))) "Show XML data")
   (:inputtext :multiline "true"
	       :width (:constraint nil canvas.width)
	       :bgcolor "0xa0a0a0"
	       :id "t"
	       :height 300)
   (:debug)))

(use-package :bknr.datastore)
(use-package :bknr.indices)

(defvar *user-dtd*
  (cxml:parse-dtd-file "user.dtd"))

(define-persistent-class user (xml-store-object)
  ((first-name :update :element "firstName")
   (last-name :update :element "lastName")
   (vat-number :update :element "vatNumber")
   (email :update :index-type string-unique-index
	  :index-reader user-with-email
	  :index-values all-users
	  :element "email"))
  (:metaclass persistent-xml-class)
  (:dtd *user-dtd*)
  (:element "user"))


(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (with-slots (first-name last-name email) user
      (format stream "~a ~a (~a)" first-name last-name email))))
			 

(make-instance 'mp-store :directory "C:/Documents and Settings/manuel/My Documents/laszlo-store/"
	       :subsystems (list (make-instance 'store-object-subsystem)))

(make-object 'user
	     :first-name "Manuel"
	     :last-name "Odendahl"
	     :vat-number "23849"
	     :email "manuel@bl0rg.net")

(use-package :net.aserve)
(use-package :xml)

(publish-xml-handler 'user)

(defun start-aserve ()
  (net.aserve:start :port 8081))

(laszlo-file "modell.lzx"
  ((:canvas :height 800 :debug "true")
   
   (:include :href "xml-lib.lzx")

   ((:dataset :name "modell")
    (:profile
     (:modell
      "MacromediaDev"
      (:set "Set1"
	(:attribute "Price per Hour"
		    (:value "0-20EUR")
		    (:importance "100%"))
	(:attribute "DevLanguage"
		    (:value "MMFlash!")
		    (:importance "100%"))
	(:attribute "DevLanguage"
		    (:value "XML")
		    (:importance "50%"))
	(:attribute "# References"
		    (:value "10-100")
		    (:importance "50%")))
      (:set "Set2"
	(:attribute "Price Per Hour"
		    (:value "0-30EUR")
		    (:importance "100%"))
	(:attribute "DevLanguage"
		    (:value "MMShockwave")
		    (:importance "100%"))
	(:attribute "# References"
		    (:value "5-100")
		    (:importance "50%")))
      (:set "Set3"
        (:attribute "DigitalFulfillment"
		    (:value "Yes")
		    (:importance "100%"))
	(:attribute "Hotline"
		    (:value "Yes")
		    (:importance "70%")))

      (:set "Set4"
	(:attribute "Size"
		    (:value "0-10sqm")
		    (:importance "80%"))))))

   ((:class :name "attributeView" :extends "view" :height (:constraint () parent.height) :width 120)
    (:text :y 10 :name "name" :datapath "text()" :bgcolor "#CDCDCD")
    (:text :y 35 :name "value" :datapath "value/text()")
    (:text :y 60 :name "importance" :datapath "importance/text()"))

   ((:class :name "setView" :extends "view" :height 100 :width 400)
    (:simplelayout :axis "x" :spacing 10)
    (:view
	    
     (:text :y 10 :name "name" :datapath "text()" :bgcolor "#BDBDBD"
	    :onclick (progn (parent.edit-button.set-visible (not parent.edit-button.visible))
			    (parent.delete-button.set-visible (not parent.delete-button.visible))))
     ((:button :name "editButton" :visible "false"
	       :width 80 :x 10 :y 35) "Edit..."
      (:jsevent onclick ()
		(-debug.write "edit set")))
     ((:button :name "deleteButton" :visible "false"
	       :width 80 :x 10 :y 60) "Delete..."
      (:jsevent onclick ()
		(-debug.write "delete set"))))
      
    (:attribute-view :datapath "attribute"))

   (:view :x 10
    (:simplelayout :axis "y")
    (:set-view :datapath "modell:/profile/modell[1]/set")
    ((:button :name "addButton" :width 100
	      :onclick (-debug.write "new set")) "Add new set..."))))

(defparameter *attribute-dtd*
  (cxml:parse-dtd-file "attribute.dtd"))

(define-persistent-class attribute (xml-store-object)
  ((name :update :body t)
   (value :update :element "value")
   (importance :update :element "importance"))
  (:metaclass persistent-xml-class)
  (:dtd *attribute-dtd*)
  (:element "attribute"))

(defmethod print-object ((attr attribute) stream)
  (print-unreadable-object (attr stream :type t)
    (with-slots (name value importance) attr
      (format stream "~S: ~A (~A)" name value importance))))

(defun all-attributes ()
  (store-objects-with-class 'attribute))

(make-object 'attribute :name "test-attribute" :value "10sqm" :importance "100%")

(define-persistent-class constraint-set (xml-store-object)
  ((name :update :body t)
   (attributes :update :element "attribute"))
  (:metaclass persistent-xml-class)
  (:dtd *attribute-dtd*)
  (:element "set"))

(defmethod print-object ((set constraint-set) stream)
  (print-unreadable-object (set stream :type t)
    (with-slots (name attributes) set
      (format stream "~S: ~A attrs" name (length attributes)))))

(defun all-sets ()
  (store-objects-with-class 'constraint-set))

(make-object 'constraint-set :name "set1"
	     :attributes (all-attributes))