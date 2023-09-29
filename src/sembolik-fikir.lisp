(defpackage :sembolik-fikir
  (:nicknames :sf)
  (:use #:cl
        #:named-readtables
        #:sf/reader)
  (:export #:sf-file
           #:rpl
           #:repl))

(in-package :sf)

(defun reader-macro-handler (stream char)
  (declare (ignore char))
  (transform (read-source-code stream)))

(defreadtable sf-readtable
  (:macro-char #\> #'reader-macro-handler))

(define-symbol-macro sf-file
    (progn
      (in-package :sf)
      (in-readtable sf-readtable)))

(import '(sf:sf-file) :cl-user)
