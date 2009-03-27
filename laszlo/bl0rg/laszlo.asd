(in-package :cl-user)

(defpackage :laszlo.system
  (:use :cl :asdf))

(in-package :laszlo.system)

(defsystem :laszlo
    :name "Laszlo"
    :author "Manuel Odendahl <manuel@bl0rg.net>"
    :version "0"
    :maintainer "Manuel Odendahl <manuel@bl0rg.net>"
    :licence "BSD"
    :description "OpenLaszlo stuff"

    :depends-on (:bknr.datastore :bknr.data.impex :bknr.impex :xhtmlgen)
;;		 #-(or allegro) :htmlgen  )

    :components ((:file "package")
		 (:file "xml" :depends-on ("package"))
		 (:file "utils" :depends-on ("package"))
                 (:file "macros" :depends-on ("utils" "package"))
		 (:file "js" :depends-on ("package" "utils" "macros"))
		 (:file "js-html" :depends-on ("package" "js" "utils"))
		 (:file "js-xml" :depends-on ("package" "js" "utils"))
		 (:file "css" :depends-on ("package" "utils"))
		 (:file "web" :depends-on ("package" "xml" "js"))
		 (:file "laszlo" :depends-on ("package" "xml" "js"))))
