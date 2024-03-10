(defpackage :sembolik-fikir
  (:nicknames :sf)
  (:use #:cl
        #:named-readtables
        #:sf/reader)
  (:shadow #:*
           #:/
           #:+
           #:-
           #:=
           #:/=
           #:>
           #:<
           #:>=
           #:<=
           #:1+
           #:1-)
  (:export #:sf

           #:|olsun|
           #:|olsun:|
           #:|=:|
           #:|sözdizimi-olsun:|

           #:read-source-code
           #:transform

           #:rpl
           #:repl))

(in-package :sf)

(defvar *print-transformed-expr* nil)

(defun reader-macro-handler (stream char)
  (declare (ignore char))
  (let ((expr (transform (read-source-code stream))))
    (when *print-transformed-expr* (pprint expr))
    expr))

(defreadtable sf-readtable
  (:merge :common-lisp)
  (:macro-char #\> #'reader-macro-handler))

(define-symbol-macro sf
    (progn
      (in-package :sf)
      (in-readtable sf-readtable)))

(use-package '(:sf) :cl-user)
