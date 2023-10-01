(defpackage :sembolik-fikir/reader
  (:nicknames :sf/reader)
  (:use #:cl
        #:split-sequence
        #:alexandria
        #:iterate)
  (:export #:|olsun|
           #:|olsun:|

           #:read-source-code
           #:transform

           #:rpl
           #:repl))

(in-package :sf/reader)

(defstruct phrase base extension)
;;  araba'dan
;; "yazı"'nın
;;      5'in
;; <base>'<extension>

(defstruct expression phrases action)
;; istanbul'dan ankara'ya git.
;; <phrases> <action>.

(defstruct action symbol parameter)
;; git
;; <symbol>
;;
;; yap: [ t | t'yi ayarla. ]
;; <symbol>: <parameter>

(defstruct code-block body)
;; ( 1 ekle: 5. 10'un karesi. )
;; ( <body> )

(defstruct procedure params body)
;;  [ sayı | sayı'nın karesi. ]
;;  [ <params> | <body> ]
;;
;;  [ 5'in karesi. ]
;;  [ <body> ]


;; `reader-input-stream' implementation to keep track of the last read char.
;; This was required to differentiate the dot character of numbers.
;; i.e. `1.' was read as `1' by Lisp reader, so we need to keep track
;; of read characters to understand that user entered `.' after `1'.
(defclass reader-input-stream (sb-gray:fundamental-character-input-stream)
  ((stream :initarg :stream
           :reader stream-of)
   (last-read-char :accessor stream-last-read-char :initform nil)
   (prev-last-read-char :accessor stream-prev-last-read-char :initform nil)))

(defmethod sb-gray:stream-read-char ((stream reader-input-stream))
  (let ((char (read-char (stream-of stream) nil :EOF)))
    (psetf (stream-prev-last-read-char stream) (stream-last-read-char stream)
           (stream-last-read-char stream) char)
    char))

(defmethod sb-gray:stream-unread-char ((stream reader-input-stream) char)
  (psetf (stream-last-read-char stream) (stream-prev-last-read-char stream)
         (stream-prev-last-read-char stream) nil)
  (unread-char char (stream-of stream)))
;; End of `reader-input-stream' implementation.

(defmacro with-reader-input-stream (stream-sym &body body)
  `(let* ((,stream-sym (if (eq (type-of ,stream-sym) 'reader-input-stream)
                           ,stream-sym
                           (make-instance 'reader-input-stream
                                          :stream ,stream-sym))))
     ,@body))

(defmacro defreader ((reader-name stream-sym) &body iterate-body)
  `(defun ,reader-name (&optional (,stream-sym *standard-input*))
     (with-reader-input-stream ,stream-sym
       (iterate ,@iterate-body))))

(defconstant +package-delimiter+ #\/)
(defvar +whitespace-chars+ '(#\newline #\space #\tab))
(defvar +end-of-expression+ '(:EOF #\.))
(defun read-next-char (stream)
  (read-char stream nil :EOF))
(defun peek-next-char (stream)
  (peek-char nil stream nil :EOF))
(defun read-lisp-object (stream)
  (let ((*readtable* (named-readtables:find-readtable :common-lisp)))
    (read-preserving-whitespace stream nil :EOF)))
(defun bool-value (thing) (not (not thing)))
(defun phrase-is-parametric? (phrase)
  (with-slots (base) phrase
    (and (symbolp base)
         (bool-value (ends-with #\: (symbol-name base))))))

(defun intern-symbol (word)
  (let* ((parts (split-sequence +package-delimiter+ word)))
    (if (= (length parts) 1)
        (intern word)
        (find-symbol-case-insensitive (second parts) (first parts)))))

(defun find-symbol-case-insensitive (symbol-name package-name)
  (let ((package (find-package-case-insensitive package-name)))
    (or (find-symbol symbol-name package)
        (find-symbol (string-upcase symbol-name) package))))

(defun find-package-case-insensitive (package-name)
  (or (find-package package-name)
      (find-package (string-upcase package-name))))

(defun rpl ()
  (loop (format t "~A~%" (transform (read-expression)))))

(defun repl ()
  (loop (format t "~A~%" (eval (transform (read-expression))))))

(defun read-source-code% (string)
  (with-input-from-string (s string)
    (read-source-code s)))


(defreader (discard-whitespace stream)
  (while (member (peek-next-char stream) +whitespace-chars+))
  (read-next-char stream)
  (finally (return nil)))


(defreader (read-source-code stream)
  (for expr = (read-expression stream))
  (until (not expr))
  (collect expr))


(defreader (read-expression stream)
  (for next-char = (peek-next-char stream))

  (when (member next-char +end-of-expression+)
    (read-next-char stream)
    (finish))

  (when (and (lastcar expr)
             (numberp (phrase-base (lastcar expr)))
             (char= #\. (stream-last-read-char stream)))
    (finish))

  (collect (cond
             ((member next-char +whitespace-chars+)
              (discard-whitespace stream)
              (next-iteration))
             (t (read-phrase stream)))
    into expr)

  (finally
   (case (length expr)
     (0 (return nil))
     (1 (return (make-expression :phrases expr))))
   (let* ((possible-parametric-action-index (- (length expr) 2))
          (possible-parametric-action (nth possible-parametric-action-index expr))
          (is-action-parametric? (and (phrase-p possible-parametric-action)
                                      (phrase-is-parametric? possible-parametric-action)))
          (action-position (if is-action-parametric?
                               possible-parametric-action-index
                               (1- (length expr))))
          (phrases (subseq expr 0 action-position))
          (action-phrase (nth action-position expr))
          (action (make-action :symbol (phrase-base action-phrase)
                               :parameter (when is-action-parametric?
                                            (phrase-base (lastcar expr))))))
     (return (make-expression :phrases phrases :action action)))))


(defreader (read-phrase stream)
  (with is-extension? = nil)
  (with base = nil)

  (for next-char = (peek-next-char stream))

  (until (or (member next-char +end-of-expression+)
             (member next-char +whitespace-chars+)))

  (cond
    ((equal next-char #\')
     (read-next-char stream)
     (setf is-extension? t)
     (next-iteration))

    ((first-iteration-p)
     (setf base (cond
                  ((find next-char "0123456789\"#")
                   (read-lisp-object stream))
                  ((equal next-char #\[)
                   (read-procedure stream))
                  ((equal next-char #\()
                   (read-group-of-expressions stream))
                  (t nil)))
     (next-iteration)))

  (if is-extension?
      (collect (read-next-char stream) into extension result-type string)
      (collect (read-next-char stream) into word result-type string))

  (finally
   (return
     (make-phrase :base (or base (intern-symbol word))
                  :extension (if (string= extension "")
                                 nil
                                 extension)))))


(defreader (read-procedure stream)
  (with paranthesis-depth = 0)
  (with after-vertical-bar? = nil)
  (for next-char = (peek-next-char stream))

  (when (and (equal next-char #\])
             (= paranthesis-depth 0))
    (read-next-char stream)
    (finish))

  (cond
    ((equal next-char #\[)
     (if (first-iteration-p)
         (progn
           (read-next-char stream)
           (next-iteration))
         (incf paranthesis-depth)))

    ((and (equal next-char #\|)
          (= paranthesis-depth 0))
     (setf after-vertical-bar? t)
     (read-next-char stream)
     (next-iteration))

    ((equal next-char #\])
     (decf paranthesis-depth)))

  (if after-vertical-bar?
      (collect (read-next-char stream) into second-part result-type string)
      (collect (read-next-char stream) into first-part result-type string))

  (finally
   (return (if after-vertical-bar?
               (make-procedure :params (mapcar #'(lambda (param)
                                                   (with-input-from-string (p param)
                                                     (read-phrase p)))
                                               (split-sequence #\space
                                                               first-part
                                                               :remove-empty-subseqs t))
                               :body (read-source-code% second-part))
               (make-procedure :params nil :body (read-source-code% first-part))))))


(defreader (read-group-of-expressions stream)
  (with paranthesis-depth = 0)
  (for next-char = (peek-next-char stream))

  (when (and (equal next-char #\))
             (= paranthesis-depth 0))
    (read-next-char stream)
    (finish))

  (cond
    ((equal next-char #\()
     (if (first-iteration-p)
         (progn
           (read-next-char stream)
           (next-iteration))
         (incf paranthesis-depth)))

    ((equal next-char #\))
     (decf paranthesis-depth)))

  (collect (read-next-char stream) into result result-type 'string)

  (finally
   (return (make-code-block :body (read-source-code% result)))))
