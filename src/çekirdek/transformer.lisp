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
  (with-slots (actions phrases) expr
    (if (not actions)
        (if (and (= (length phrases) 1)
                 (phrase-extension (first phrases)))
            (transform-expr/single-phrase-with-extension (first phrases))
            (transform phrases))
        (case (action-symbol (first actions))
          ((|olsun| |olsun:| |=:|)
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
  (with-slots (actions phrases) expr
    (let* ((single-action-symbol (and (= 1 (length actions))
                                      (action-symbol (first actions))))
           (single-action-lisp-symbol? (and single-action-symbol
                                            (action-lisp-symbol? (first actions)))))
      (if single-action-lisp-symbol?
          (ensure-symbol-without-dot-at-end single-action-symbol)
          (intern
           (format nil "~{~A~}~{'~A~}"
                   (mapcar #'(lambda (a)
                               (symbol-name (action-symbol a)))
                           actions)
                   (mapcar #'(lambda (p)
                               (let ((extension (phrase-extension p)))
                                 (if (null extension)
                                     ""
                                     (extension-canonical-form extension))))
                           phrases))
           (symbol-package (action-symbol (first actions))))))))

(defun ensure-symbol-without-dot-at-end (symbol)
  (let ((symbol-name (string-right-trim '(#\.) (symbol-name symbol))))
    (find-symbol symbol-name (symbol-package symbol))))

(defun get-function-lambda-list-from-expression (expr)
  (with-slots (actions phrases) expr
    (let* ((parametric? (some #'action-parameter actions)))
      (mapcar #'transform
              (append (when parametric?
                        (mapcar #'action-parameter actions))
                      phrases)))))

(defun transform-expr/call (expr)
  (with-slots (actions phrases) expr
    (let* ((parametric? (some #'action-parameter actions))
           (single-action-symbol (and (not parametric?)
                                      (action-symbol (first actions)))))
      (if (and (not phrases)
               (not parametric?))
          `(if (fboundp ',single-action-symbol)
               (,single-action-symbol)
               ,single-action-symbol)
          `(,(get-function-symbol-from-expression expr)
            ,@(get-function-lambda-list-from-expression expr))))))

(defun transform-expr/sözdizimi-olsun (expr)
  (let* ((action (first (expression-actions expr)))
         (phrases (expression-phrases expr))
         (parameter (action-parameter action))
         (action-parameter-base (when parameter (phrase-base parameter)))
         (phrase-bases (phrase-bases phrases)))
    (assert (code-block-p (first phrase-bases)))
    (assert (code-block-p action-parameter-base))
    (let ((from-expr (first (code-block-body (first phrase-bases)))))
      (let ((macro-symbol (get-function-symbol-from-expression from-expr)))
        `(progn
           (export ',macro-symbol (symbol-package ',macro-symbol))
           (defmacro ,macro-symbol
               ,(get-function-lambda-list-from-expression from-expr)
             ,(transform (code-block-body action-parameter-base))))))))

(defun transform-expr/olsun (expr)
  (let* ((action (first (expression-actions expr)))
         (phrases (expression-phrases expr))
         (action-parameter (action-parameter action))
         (action-parameter-base (when action-parameter
                                  (phrase-base action-parameter)))
         (phrases-length (length phrases))
         (phrase-bases (phrase-bases phrases)))
    (cond
      ((and (= phrases-length 1)
            (code-block-p (first phrase-bases))
            (code-block-p action-parameter-base))
       (transform-expr/olsun/defun (first (code-block-body (first phrase-bases)))
                                   (code-block-body action-parameter-base)))
      ((and (> phrases-length 1)
            (member (type-of action-parameter-base) '(code-block procedure)))
       (typecase action-parameter-base
         (code-block (transform-expr/olsun/let phrase-bases action-parameter-base))
         (procedure (transform-expr/olsun/let phrase-bases
                                              (make-code-block :body (list action-parameter-base)))))
       )
      ((and (not action-parameter)
            (= phrases-length 2)
            (symbolp (first phrase-bases)))
       (transform-expr/olsun/defvar (first phrase-bases) (second phrases)))
      ((and action-parameter
            (= phrases-length 1)
            (symbolp (first phrase-bases)))
       (transform-expr/olsun/defvar (first phrase-bases) action-parameter))
      (t
       (error "no matching 'olsun' pattern")))))

(defun transform-expr/olsun/defvar (var-phrase value-phrase)
  (let ((param-symbol (transform var-phrase)))
    `(progn
       (export ',param-symbol (symbol-package ',param-symbol))
       (defparameter ,param-symbol
         ,(transform value-phrase)))))

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
       (setf (get ',function-symbol :interface) ,(read-procedure-signature-from-expression expr))
       (export ',function-symbol (symbol-package ',function-symbol))
       (defun ,function-symbol (,@(get-function-lambda-list-from-expression expr))
         ,(transform body)))))

(defun phrase-bases (phrases)
  (mapcar #'phrase-base phrases))

(defun read-procedure-signature-from-expression (expr)
  (let ((exp-string
          (with-output-to-string (out)
            (dolist (phrase (expression-phrases expr))
              (format out "~A~@['~A~] " (phrase-base phrase)
                      (phrase-extension phrase)))
            (dolist (action (expression-actions expr))
              (format out "~A~@[ ~A~] " (action-symbol action)
                      (let ((param-phrase (action-parameter action)))
                        (when param-phrase
                          (phrase-base param-phrase))))))))
    (string-trim '(#\space) exp-string)))

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
