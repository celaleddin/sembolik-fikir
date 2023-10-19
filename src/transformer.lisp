(in-package :sf/reader)

(defgeneric transform (thing))

(defun transform-single-or-multiple (list)
  (cond
    ((null list) nil)
    ((= (length list) 1)
     (transform (first list)))
    (t `(progn ,@(mapcar #'transform list)))))

(defmethod transform (thing) thing)

(defmethod transform ((thing list))
  (transform-single-or-multiple thing))

(defmethod transform ((phrase phrase))
  (with-slots (base base-lisp-expr?) phrase
    (if base-lisp-expr?
        base
        (transform base))))

(defmethod transform ((expr expression))
  (with-slots (action phrases) expr
    (if (not action)
        (if (and (= (length phrases) 1)
                 (phrase-extension (first phrases)))
            (transform-expr/single-phrase-with-extension (first phrases))
            (transform phrases))
        (case (action-symbol action)
          ((|olsun| |olsun:|)
           (transform-expr/olsun expr))
          (|sözdizimi-olsun:|
           (transform-expr/sözdizimi-olsun expr))
          (otherwise
           (transform-expr/call expr))))))

(defmethod transform ((code-block code-block))
  (transform-single-or-multiple (code-block-body code-block)))

(defmethod transform ((proc procedure))
  (with-slots (params body) proc
    `(lambda (,@(mapcar #'transform params))
       ,@(mapcar #'transform body))))

;; Helpers

(defun transform-expr/single-phrase-with-extension (phrase)
  (with-slots (base extension) phrase
    (if (string= extension "")
        base
        (list (transform base) extension))))

(defun get-function-symbol-from-expression (expr)
  (with-slots (action phrases) expr
    (with-slots ((action-symbol symbol)
                 (action-lisp-symbol? lisp-symbol?))
        action
      (if action-lisp-symbol?
          (ensure-symbol-without-dot-at-end action-symbol)
          (intern
           (format nil "~A~{/~A~}"
                   (symbol-name action-symbol)
                   (mapcar #'(lambda (p)
                               (let ((extension (phrase-extension p)))
                                 (if (null extension)
                                     ""
                                     (extension-canonical-form extension))))
                           phrases))
           (symbol-package action-symbol))))))

(defun ensure-symbol-without-dot-at-end (symbol)
  (let ((symbol-name (string-right-trim '(#\.) (symbol-name symbol))))
    (find-symbol symbol-name (symbol-package symbol))))

(defun get-function-lambda-list-from-expression (expr)
  (with-slots (action phrases) expr
    (with-slots (parameter) action
      (append (if parameter (list (transform parameter)) '())
              (mapcar #'transform phrases)))))

(defun transform-expr/call (expr)
  (with-slots (action phrases) expr
    (with-slots (symbol parameter) action
      (if (and (not phrases)
               (not (action-parameter action)))
          (let ((fun-sym (gensym "fun-")))
            `(let ((,fun-sym (handler-case (function ,symbol)
                               (undefined-function ()
                                 (lambda () ,symbol)))))
               (funcall ,fun-sym)))
          `(,(get-function-symbol-from-expression expr)
            ,@(get-function-lambda-list-from-expression expr))))))

(defun transform-expr/sözdizimi-olsun (expr)
  (let* ((action (expression-action expr))
         (phrases (expression-phrases expr))
         (parameter (action-parameter action))
         (phrase-bases (phrase-bases phrases)))
    (assert (code-block-p (first phrase-bases)))
    (assert (code-block-p parameter))
    (let ((from-expr (first (code-block-body (first phrase-bases)))))
      `(defmacro ,(get-function-symbol-from-expression from-expr)
           ,(get-function-lambda-list-from-expression from-expr)
         ,(transform (code-block-body parameter))))))

(defun transform-expr/olsun (expr)
  (let* ((action (expression-action expr))
         (phrases (expression-phrases expr))
         (parameter (action-parameter action))
         (phrases-length (length phrases))
         (phrase-bases (phrase-bases phrases)))
    (cond
      ((and (= phrases-length 1)
            (code-block-p (first phrase-bases))
            (code-block-p parameter))
       (transform-expr/olsun/defun (first (code-block-body (first phrase-bases)))
                                   (code-block-body parameter))
       )
      ((and (> phrases-length 1)
            (member (type-of parameter) '(code-block procedure)))
       (typecase parameter
         (code-block (transform-expr/olsun/let phrase-bases parameter))
         (procedure (transform-expr/olsun/let phrase-bases
                                              (make-code-block :body (list parameter)))))
       )
      ((and (not parameter)
            (= phrases-length 2)
            (symbolp (first phrase-bases)))
       (transform-expr/olsun/defvar (first phrase-bases) (second phrase-bases)))
      ((and parameter
            (= phrases-length 1)
            (symbolp (first phrase-bases)))
       (transform-expr/olsun/defvar (first phrase-bases) parameter))
      (t
       (error "no matching 'olsun' pattern")))))

(defun transform-expr/olsun/defvar (var-phrase value-phrase)
  `(defparameter ,(transform var-phrase)
     ,(transform value-phrase)))

(defun transform-expr/olsun/let (bindings body)
  `(let* (,@(iterate
              (declare (ignorable rest))
              (for (sym val . rest)
                   :on (mapcar #'transform bindings)
                   :by #'cddr)
              (collect `(,sym ,val))))
     ,(transform body)))

(defun transform-expr/olsun/defun (expr body)
  (let ((function-symbol (get-function-symbol-from-expression expr)))
    `(defun ,function-symbol (,@(get-function-lambda-list-from-expression expr))
       ,(transform body))))

(defun phrase-bases (phrases)
  (mapcar #'phrase-base phrases))

;; x 5 olsun.
;; y olsun: 10.
;; pi 3.14 sabit-olsun.
;; y (x +: 10) olsun.

;; setf
;; (x'in 2'nci elemanı) 10 olsun.

;; defun / (setf symbol-function):
;;
;; (x +: y) olsun: (
;;   x -: y.
;; )

;; let:
;;
;; a 1
;; b 2 olsun: (
;;   a +: b
;; )


;; flet / labels:
;;
;; (a'dan b'ye) (b'den a'ya) prosedür-olsun: (
;; )
;; (p-olsun?)

;; let over lambda:
;; t (a 1
;;    b 2 olsun: [ x |
;;      ( a +: b ) *: x.
;;    ]
;; ) olsun.

;; (x'in y'inci elemanı) olsun: (
;;   (nth x y)
;; )

;; (x y sabit-olsun) sözdizimi-olsun: (
;;   (defconstant ,x ,y)
;; ).

;; procedure without action
;; x'ten. y'ye.
