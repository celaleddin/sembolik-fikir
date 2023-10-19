(defclass sf-source-file (asdf:cl-source-file)
  ((type :initform "sf")))

(defsystem :sembolik-fikir
  :description "A programming language in Turkish"
  :author "Celaleddin Hidayetoglu <celaleddin@sembolik-fikir.com>"
  :depends-on (#:iterate
               #:alexandria
               #:named-readtables
               #:cl-ppcre
               #:split-sequence)
  :in-order-to ((test-op (test-op :sembolik-fikir/tests)))
  :components ((:module "core"
                :pathname "src"
                :serial t
                :components ((:file "reader")
                             (:file "extension")
                             (:file "transformer")))
               (:file "src/sembolik-fikir" :depends-on ("core"))
               (:module "sf"
                :pathname "src/sf"
                :depends-on ("src/sembolik-fikir")
                :default-component-class sf-source-file
                :serial t
                :components ((:file "utils")))))

(defsystem :sembolik-fikir/tests
  :description "Sembolik fikir tests"
  :author "Celaleddin Hidayetoglu <celaleddin@sembolik-fikir.com>"
  :depends-on (#:sembolik-fikir
               #:fiveam)
  :perform (test-op (op c) (symbol-call :sf/tests :run-tests))
  :pathname "tests/"
  :components ((:file "package")
               (:file "transform")))
