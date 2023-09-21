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
       (transform (first (expression-phrases expr))))
      ((string= (action-name action) "olsun")
       (transform-expr/olsun expr))
      (t `(,(get-function-name-from-expression expr) ,@(mapcar #'transform phrases))))))

(defun get-function-name-from-expression (expr)
  (with-slots (action phrases) expr
    (intern (format nil "~A/~{~A~^/~}"
                    (action-name action) (mapcar #'phrase-extension phrases))
            :keyword)))

(defmethod transform ((code-block code-block))
  (transform-single-or-multiple (code-block-body code-block)))

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
;; (a'dan b'ye) (b'den a'ya) olsun: ([
;;   ; interesting
;; ])

;; let over lambda:
;; t (a 1
;;    b 2 olsun: [ x |
;;      ( a +: b ) *: x.
;;    ]
;; ) olsun.

;; (x'in y'inci elemanı) olsun: (
;;   (nth x y)
;; )
