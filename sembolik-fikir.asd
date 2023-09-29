(defclass sf-source-file (asdf:cl-source-file)
  ((type :initform "sf")))

(defsystem :sembolik-fikir
  :description "A programming language in Turkish"
  :author "Celaleddin Hidayetoglu <celaleddin@sembolik-fikir.com>"
  :depends-on (#:iterate
               #:alexandria
               #:named-readtables
               #:split-sequence)
  :components ((:module "core"
                :pathname "src"
                :serial t
                :components ((:file "reader")
                             (:file "transformer")))
               (:file "src/sembolik-fikir" :depends-on ("core"))
               (:module "sf"
                :pathname "src/sf"
                :serial t
                :depends-on ("src/sembolik-fikir")
                :default-component-class sf-source-file
                :components ((:file "test")))))

(defsystem :sembolik-fikir/tests
  :description "Sembolik fikir tests"
  :author "Celaleddin Hidayetoglu <celaleddin@sembolik-fikir.com>"
  :depends-on (#:sembolik-fikir)
  :pathname "test/"
  :components ((:file "package")
               (:file "reader")))
