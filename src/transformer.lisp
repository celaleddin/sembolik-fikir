(defpackage sembolik-fikir/transformer
  (:nicknames :sf/transformer)
  (:use #:cl
        #:sf/reader)
  (:export #:transform))

(in-package #:sf/transformer)

(defgeneric transform (thing))

(defmethod transform (thing) thing)
(defmethod transform ((thing list))
  `(progn ,@(mapcar #'transform thing)))

(defmethod transform ((phrase phrase))
  (with-slots (word is-word-lisp-object?) phrase
    (if is-word-lisp-object?
        word
        (intern word))))

(defmethod transform ((expr expression))
  (with-slots (action phrases) expr
    (cond
      ((not action)
       (transform (car (expression-phrases expr))))
      ((string= (action-name action) "olsun")
       (transform-expr/olsun expr))
      (t (let ((action-symbol (intern (action-name action))))
           `(,action-symbol ,@(mapcar #'transform phrases)))))))

(defmethod transform ((code-block code-block))
  `(progn ,@(mapcar #'transform (code-block-body code-block))))

(defmethod transform ((proc procedure))
  (with-slots (params body) proc
    `(lambda (,@(mapcar #'transform params))
       ,@(mapcar #'transform body))))

(defun transform-expr/olsun (expression)
  (with-slots (action phrases) expression
    (with-slots (parameter) action
      (cond
        ((not parameter)
         `(setf ,@(mapcar #'transform phrases)))
        ((and (= (length phrases) 1)
              (code-block-p (first phrases))
              (code-block-p parameter))
         `(defun ,(action-name (expression-action (first (code-block-body (first phrases))))) ()
            ,(transform parameter)))))))
