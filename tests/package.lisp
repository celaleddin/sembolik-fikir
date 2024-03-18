(defpackage #:sembolik-fikir/tests
  (:nicknames #:sf/tests)
  (:use #:cl
        #:sf
        #:fiveam)
  (:export #:run-tests))

(in-package :sf/tests)

(defmacro sf (sf-string)
  (let ((stream-sym (gensym "stream")))
    `(with-input-from-string (,stream-sym ,sf-string)
       (transform (read-source-code ,stream-sym)))))

(defmacro is-equal (thing1 thing2)
  `(is (equal ,thing1 ,thing2)))

(defmacro is-all-equal (&body pairs)
  `(progn
     ,@(mapcar (lambda (pair)
                 `(is-equal ,(car pair) ,(cadr pair)))
               pairs)))

(defun run-tests ()
  (run! 'sf-tests))

(defun eval-sf (sf-string)
  (eval (sf sf-string)))

(defun subexpr? (lisp-expr sf-code)
  (member lisp-expr (sf sf-code) :test #'equal))

(def-suite sf-tests
  :description "sembolik-fikir test suite")
