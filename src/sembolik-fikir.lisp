(defpackage :sembolik-fikir
  (:nicknames :sf)
  (:use #:cl
        #:named-readtables
        #:sf/reader)
  (:shadow #:*
           #:/
           #:+
           #:-)
  (:export #:sf

           #:|olsun|
           #:|olsun:|
           #:|sözdizimi-olsun:|

           #:read-source-code
           #:transform

           #:rpl
           #:repl))

(in-package :sf)

(defun reader-macro-handler (stream char)
  (declare (ignore char))
  (transform (read-source-code stream)))

(defreadtable sf-readtable
  (:merge :common-lisp)
  (:macro-char #\> #'reader-macro-handler))

(define-symbol-macro sf
    (progn
      (in-package :sf)
      (in-readtable sf-readtable)))

(use-package '(:sf) :cl-user)
