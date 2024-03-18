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
  :components ((:module "çekirdek"
                :pathname "src/çekirdek"
                :serial t
                :components ((:file "reader")
                             (:file "extension")
                             (:file "transformer")))
               (:file "src/sembolik-fikir" :depends-on ("çekirdek"))
               (:module "sf"
                :pathname "src"
                :depends-on ("src/sembolik-fikir")
                :default-component-class sf-source-file
                :serial t
                :components ((:file "sözdizimi")
                             (:file "akış")
                             (:file "araçlar")
                             (:file "prosedür")
                             (:file "mantık")
                             (:file "matematik")
                             (:file "liste")))))

(defsystem :sembolik-fikir/tests
  :description "Sembolik fikir tests"
  :author "Celaleddin Hidayetoglu <celaleddin@sembolik-fikir.com>"
  :depends-on (#:sembolik-fikir
               #:fiveam)
  :perform (test-op (op c) (symbol-call :sf/tests :run-tests))
  :pathname "tests/"
  :components ((:file "package")
               (:file "transform")
               (:file "akış")))
