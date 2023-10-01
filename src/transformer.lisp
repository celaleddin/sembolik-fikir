(in-package :sf/reader)

(defgeneric transform (thing))

(defun transform-single-or-multiple (list)
  (if (= (length list) 1)
      (transform (first list))
      `(progn ,@(mapcar #'transform list))))

(defmethod transform (thing) thing)

(defmethod transform ((thing list))
  (transform-single-or-multiple thing))

(defmethod transform ((phrase phrase))
  (with-slots (base) phrase
    (typecase base
      (symbol base)
      (t (transform base)))))

(defmethod transform ((expr expression))
  (with-slots (action phrases) expr
    (cond
      ((not action)
       (if (and (= (length phrases) 1)
                (phrase-extension (first phrases)))
           (transform-expr/single-phrase-with-extension (first phrases))
           (transform phrases)))
      ((member (action-symbol action) '(|olsun| |olsun:|))
       (transform-expr/olsun expr))
      (t `(,(get-function-symbol-from-expression expr)
           ,@(get-function-lambda-list-from-expression expr))))))

(defmethod transform ((code-block code-block))
  (transform-single-or-multiple (code-block-body code-block)))

(defmethod transform ((proc procedure))
  (with-slots (params body) proc
    `(lambda (,@(mapcar #'transform params))
       ,@(mapcar #'transform body))))

;; Helpers

(defun transform-expr/single-phrase-with-extension (phrase)
  (with-slots (base extension) phrase
    (list (transform base) extension)))

(defun get-function-symbol-from-expression (expr)
  (with-slots (action phrases) expr
    (let ((action-symbol (action-symbol action)))
      (if (not (get action-symbol :sf-fun))
          action-symbol
          (intern (format nil "~A/~{~A~^/~}"
                          (symbol-name action-symbol)
                          (mapcar #'(lambda (p) (or (phrase-extension p) ""))
                                  phrases))
                  (symbol-package action-symbol))))))

(defun get-function-lambda-list-from-expression (expr)
  (with-slots (action phrases) expr
    (with-slots (parameter) action
      (append (if parameter (list (transform parameter)) '())
              (mapcar #'transform phrases)))))

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
  `(defvar ,(transform var-phrase)
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
    `(progn
       (defun ,function-symbol (,@(get-function-lambda-list-from-expression expr))
         ,(transform body))
       (setf (get ',function-symbol :sf-fun) t))))

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
