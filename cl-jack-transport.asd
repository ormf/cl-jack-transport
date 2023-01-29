;;;; cl-jack-transport.asd

(asdf:defsystem #:cl-jack-transport
  :description "cffi bindings for jack_transport"
  :author "Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>"
  :license  "gpl 2.0 or later"
  :version "0.0.1"
  :depends-on (#:cffi #:bordeaux-threads #:orm-utils)
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "transport")
               (:file "jack")
               (:file "callbacks")
               (:file "jack-transport")))
