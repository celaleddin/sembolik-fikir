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
  (with-slots (base extension) phrase
    (let ((thing (typecase base
                   (symbol base)
                   (t (transform base)))))
      (if extension
          `(sf/reader:extension-handler ,extension ,thing)
          thing))))

(defgeneric extension-handler (extension thing))
(defmethod extension-handler (extension thing)
  (list thing extension))

(defmethod transform ((expr expression))
  (with-slots (action phrases) expr
    (cond
      ((not action)
       (transform phrases))
      ((string= (action-name action) "olsun")
       (transform-expr/olsun expr))
      (t `(,(get-function-name-from-expression expr) ,@(mapcar #'transform phrases))))))

(defun get-function-name-from-expression (expr)
  (with-slots (action phrases) expr
    (intern (format nil "~A/~{~A~^/~}"
                    (action-name action) (mapcar #'phrase-extension phrases)))))

(defmethod transform ((code-block code-block))
  (transform-single-or-multiple (code-block-body code-block)))

(defmethod transform ((proc procedure))
  (with-slots (params body) proc
    `(lambda (,@(mapcar #'transform params))
       ,@(mapcar #'transform body))))

(defun transform-expr/call (expr)
  ())

(defmacro with-expr-data (expr-sym extra-bindings &body body)
  `(let* ((action (expression-action ,expr-sym))
          (phrases (expression-phrases ,expr-sym))
          (parameter (action-parameter action))
          (phrases-length (length phrases))
          (phrase-bases (phrase-bases phrases))
          ,@extra-bindings)
     ,@body))

(defun transform-expr/olsun (expr)
  (with-expr-data expr ()
    (cond
      ((and (= phrases-length 1)
            (code-block-p (phrase-base (first phrases)))
            (code-block-p parameter))
       ;; defun
       )
      ((and (> phrases-length 1)
            (or (every #'(lambda (b)
                           (not (code-block-p b)))
                       phrase-bases)
                (every #'code-block-p phrase-bases))
            (code-block-p parameter))
       ;; let
       )
      ((not parameter)
       `(setf ,@(mapcar #'transform phrases)))
      (t
       (error "no matching 'olsun' pattern")))))

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
