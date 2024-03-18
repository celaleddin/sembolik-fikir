(defpackage :sembolik-fikir/reader
  (:nicknames :sf/reader)
  (:use #:cl
        #:split-sequence
        #:alexandria
        #:iterate
        #:cl-ppcre)
  (:export #:|olsun|
           #:|olsun:|
           #:|=:|
           #:|sözdizimi-olsun:|

           #:read-source-code
           #:read-source-code-from-string
           #:transform
           #:get-function-symbol-from-expression

           #:rpl
           #:repl))

(in-package :sf/reader)

(defstruct phrase base extension base-lisp-expr?)
;;  araba'dan
;; "yazı"'nın
;;      5'in
;; <base>'<extension>

(defstruct expression phrases actions)
;; istanbul'dan ankara'ya git.
;; <phrases> <actions>.

(defstruct action symbol parameter lisp-symbol?)
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
  (let ((char (read-char (stream-of stream) nil :eof)))
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

(defmacro defreader (reader-name (stream-sym) &body iterate-body)
  `(defun ,reader-name (&optional (,stream-sym *standard-input*))
     (with-reader-input-stream ,stream-sym
       (iterate ,@iterate-body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar +whitespace-chars+ '(#\newline #\space #\tab #\,))
  (defvar +end-of-expression-chars+ '(:eof #\. #\}))
  (defvar +package-delimiter+ #\/)
  (defvar +comment-starter+ #\;))

(defun read-next-char (stream)
  (read-char stream nil :eof))
(defun peek-next-char (stream)
  (peek-char nil stream nil :eof))
(defun read-lisp-object (stream)
  (handler-bind ((sb-int:simple-reader-package-error #'continue)
                 (sb-ext:package-locked-error #'continue))
    (let ((*readtable* (named-readtables:find-readtable :common-lisp)))
      (read-preserving-whitespace stream nil :eof))))

(defun bool-value (thing) (not (not thing)))
(defun phrase-is-parametric? (phrase)
  (with-slots (base) phrase
    (and (symbolp base)
         (bool-value (ends-with #\: (symbol-name base))))))

(defun intern-symbol (word)
  (let* ((parts (split-sequence +package-delimiter+ word
                                :remove-empty-subseqs t)))
    (if (or (not parts)
            (= (length parts) 1))
        (intern word)
        (intern (second parts) (find-package-case-insensitive (first parts))))))

(defun find-package-case-insensitive (package-name)
  (or (find-package package-name)
      (find-package (string-upcase package-name))))

(defun rpl ()
  (loop (format t "~A~%" (transform (read-expression)))))

(defun repl ()
  (loop (format t "~A~%" (eval (transform (read-expression))))))

(defun read-source-code-from-string (string)
  (with-input-from-string (s string)
    (read-source-code s)))


(defreader discard-whitespace (stream)
  (while (member (peek-next-char stream) +whitespace-chars+))
  (read-next-char stream)
  (finally (return nil)))


(defreader discard-comment (stream)
  (until (member (peek-next-char stream) '(#\Newline :eof)))
  (read-next-char stream)
  (finally (return nil)))


(defreader read-source-code (stream)
  (for expr = (read-expression stream))
  (until (not expr))
  (collect expr))


(defreader read-expression (stream)
  (for next-char = (peek-next-char stream))

  ;; to be able to read end of expression in cases:
  ;; * 3.      - number with a dot but without decimal part
  ;; * @cl:+.  - a lisp symbol with a dot at the end
  ;;             (also see function `ensure-symbol-without-dot-at-end')
  (when (and (lastcar expr)
             (or (numberp (phrase-base (lastcar expr)))
                 (phrase-base-lisp-expr? (lastcar expr)))
             (let ((last-read-char (stream-last-read-char stream)))
               (and (not (eq last-read-char :eof))
                    (char= #\. last-read-char))))
    (finish))

  (case next-char
    (#.+end-of-expression-chars+
     (read-next-char stream)
     (finish))
    (#.+whitespace-chars+
     (discard-whitespace stream)
     (next-iteration))
    (#.+comment-starter+
     (discard-comment stream)
     (next-iteration)))

  (collect (read-phrase stream) into expr)

  (finally
   (case (length expr)
     (0 (return nil))
     (1 (let ((phrase (first expr)))
          (when (or (phrase-extension phrase)
                    (not (symbolp (phrase-base phrase))))
            (return (make-expression :phrases expr))))))

   (let* ((maybe-parametric-action-index (position-if (lambda (p)
                                                        (phrase-is-parametric? p))
                                                      expr))
          (maybe-parametric-actions (and maybe-parametric-action-index
                                         (nthcdr maybe-parametric-action-index expr)))
          (action-position (or maybe-parametric-action-index
                               (1- (length expr))))
          (phrases (subseq expr 0 action-position))

          (actions (if (not maybe-parametric-action-index)
                       (let ((action-phrase (nth action-position expr)))
                         (list (make-action :symbol (phrase-base action-phrase)
                                            :lisp-symbol? (phrase-base-lisp-expr? action-phrase))))
                       (iter (for phrases on maybe-parametric-actions by #'cddr)
                         (collect (let ((action-symbol (first phrases))
                                        (parameter (second phrases)))
                                    (make-action :symbol (phrase-base action-symbol)
                                                 :parameter parameter)))))))
     (return (make-expression :phrases phrases :actions actions)))))


(defreader read-phrase (stream)
  (with is-extension? = nil)
  (with is-base-lisp-expr? = nil)
  (with base = nil)
  (with starts-with-dash = nil)

  (for next-char = (peek-next-char stream))

  (until (or (member next-char +end-of-expression-chars+)
             (member next-char +whitespace-chars+)))

  (when (and (first-iteration-p)
             (equal next-char #\-))
    (read-next-char stream)
    (setf starts-with-dash t
          next-char (peek-next-char stream)))

  (cond
    ((equal next-char #\')
     (read-next-char stream)
     (setf is-extension? t)
     (next-iteration))

    ((first-iteration-p)
     (setf base (case next-char
                  (#.(coerce "0123456789\"#" 'list)
                   (read-lisp-object stream))
                  (#\@
                   (setf is-base-lisp-expr? t)
                   (read-next-char stream)
                   (read-lisp-object stream))
                  (#\[
                   (read-procedure stream))
                  (#\{
                   (setf is-base-lisp-expr? t)
                   (read-list stream))
                  (#\(
                   (read-group-of-expressions stream))
                  (otherwise nil)))
     (next-iteration)))

  (if is-extension?
      (collect (read-next-char stream) into extension result-type string)
      (collect (read-next-char stream) into word result-type string))

  (finally
   (let ((base (if (and starts-with-dash (numberp base))
                   (- base) base))
         (word (if starts-with-dash
                   (format nil "-~A" word) word)))
     (return
       (make-phrase :base (or base (intern-symbol word))
                    :base-lisp-expr? is-base-lisp-expr?
                    :extension (if (and (not is-extension?)
                                        (string= extension ""))
                                   nil
                                   extension))))))


(defreader read-procedure (stream)
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
                               :body (read-source-code-from-string second-part))
               (make-procedure :params nil :body (read-source-code-from-string first-part))))))


(defreader read-group-of-expressions (stream)
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
   (return (make-code-block :body (read-source-code-from-string result)))))


(defreader read-list (stream)
  (for next-char = (peek-next-char stream))

  (when (equal next-char :eof)
    (restart-case (error "Listenin sonu gelmeden dosyanın sonu geldi")
      (continue ()
        :report "Olduğu kadar listele"
        :interactive continue
        (finish))))

  (cond
    ((member next-char '#.+whitespace-chars+)
     (discard-whitespace stream)
     (next-iteration))

    ((and (equal next-char #\{)
          (first-iteration-p))
     (read-next-char stream)
     (next-iteration))

    ((equal next-char #\})
     (read-next-char stream)
     (finish)))

  (collect (read-phrase stream) into phrases-list)

  (finally
   (return `(list ,@(mapcar #'transform phrases-list)))))

