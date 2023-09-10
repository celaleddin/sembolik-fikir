(defpackage sembolik-fikir/reader
  (:nicknames :sf/reader)
  (:use #:cl
        #:split-sequence
        #:iterate)
  (:export #:procedure
           #:expression
           #:action
           #:code-block
           #:phrase

           #:make-phrase
           #:make-expression
           #:make-action
           #:expression-phrases

           #:read-source-code))

(in-package #:sf/reader)

(defvar +whitespace-chars+ '(#\newline #\space #\tab))
(defun read-next-char (stream)
  (read-char stream nil :end))
(defun peek-next-char (stream)
  (peek-char nil stream nil :end))
(defun read-lisp-object (stream)
  (read-preserving-whitespace stream nil :end))
(defun discard-whitespace (stream)
  (iterate (while (member (peek-next-char stream) +whitespace-chars+))
    (read-next-char stream)
    (finally (return nil))))
(defun bool-value (thing) (not (not thing)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct procedure params body)
  (defstruct expression phrases action)
  (defstruct action name parameter)
  (defstruct code-block body)
  (defstruct phrase word extension is-word-lisp-object?))

(defun phrase-is-parametric? (phrase)
  (and (not (phrase-is-word-lisp-object? phrase))
       (bool-value (find #\: (phrase-word phrase)))))

(defun split-into-expressions (source-code)
  (labels ((trim-strings-and-remove-empty-ones (list)
             (remove-if (lambda (string)
                          (string-equal string ""))
                        (mapcar (lambda (string)
                                  (string-trim +whitespace-chars+ string))
                                list))))
    (let ((depth 0)
          (delimiter #\.))
      (trim-strings-and-remove-empty-ones
       (split-sequence-if (lambda (char)
                            (and (equal char delimiter)
                                 (= depth 0)))
                          source-code
                          :key (lambda (char)
                                 (case char
                                   ((#\[ #\() (incf depth))
                                   ((#\] #\)) (decf depth)))
                                 char)
                          :remove-empty-subseqs t)))))

(defun read-source-code (source-code-string)
  (mapcar (lambda (expr-string)
            (with-input-from-string (expr-stream expr-string)
              (read-expression expr-stream)))
          (split-into-expressions source-code-string)))

(defun read-expression (stream)
  (iterate
    (for next-char = (peek-next-char stream))
    (until (equal next-char :end))

    (collect (cond
               ((member next-char +whitespace-chars+)
                (discard-whitespace stream)
                (next-iteration))
               ((equal next-char #\[) (read-procedure stream))
               ((equal next-char #\() (read-group-of-expressions stream))
               (t (read-phrase stream)))
      into expr)

    (finally
     (when (= (length expr) 1)
       (return (make-expression :phrases expr)))
     (let* ((possible-parametric-action-index (- (length expr) 2))
            (possible-parametric-action (nth possible-parametric-action-index expr))
            (is-action-parametric? (and (phrase-p possible-parametric-action)
                                        (phrase-is-parametric? possible-parametric-action)))
            (action-position (if is-action-parametric?
                                 possible-parametric-action-index
                                 (1- (length expr))))
            (phrases (subseq expr 0 action-position))
            (action-phrase (nth action-position expr))
            (action (make-action :name (let ((action-name (phrase-word action-phrase)))
                                         (if is-action-parametric?
                                             (subseq action-name 0 (1- (length action-name)))
                                             action-name))
                                 :parameter (when is-action-parametric?
                                              (car (last expr))))))
       (return (make-expression :phrases phrases :action action))))))

(defun read-phrase (stream)
  (iterate
    (with is-extension? = nil)
    (with word-as-lisp-object = nil)
    (for next-char = (peek-next-char stream))

    (until (or (equal next-char :end)
               (member next-char +whitespace-chars+)))

    (cond
      ((equal next-char #\')
       (read-next-char stream)
       (setf is-extension? t)
       (next-iteration))

      ((and (first-iteration-p)
            (find next-char "0123456789\"#"))
       (setf word-as-lisp-object (read-lisp-object stream))
       (next-iteration)))

    (if is-extension?
        (collect (read-next-char stream) into extension result-type string)
        (collect (read-next-char stream) into word result-type string))

    (finally
     (return
       (make-phrase :word (or word-as-lisp-object word)
                    :extension (if (string= extension "")
                                   nil
                                   extension)
                    :is-word-lisp-object? (bool-value word-as-lisp-object))))))

(defun read-procedure (stream)
  (iterate
    (with paranthesis-depth = 0)
    (with after-vertical-bar? = nil)
    (for next-char = (peek-next-char stream))

    (until (and (equal next-char #\])
                (= paranthesis-depth 1)))

    (cond
      ((equal next-char #\[)
       (incf paranthesis-depth)
       (read-next-char stream)
       (next-iteration))

      ((and (equal next-char #\|)
            (= paranthesis-depth 1))
       (setf after-vertical-bar? t)
       (read-next-char stream)
       (next-iteration))

      ((equal next-char #\])
       (decf paranthesis-depth)
       (read-next-char stream)
       (next-iteration)))

    (if after-vertical-bar?
        (collect (read-next-char stream) into second-part result-type string)
        (collect (read-next-char stream) into first-part result-type string))

    (finally
     (read-next-char stream) ; read closing paranthesis
     (return (if after-vertical-bar?
                 (make-procedure :params (mapcar #'(lambda (param)
                                                     (with-input-from-string (p param)
                                                       (read-phrase p)))
                                                 (split-sequence #\space
                                                                 first-part
                                                                 :remove-empty-subseqs t))
                                 :body (read-source-code second-part))
                 (make-procedure :params nil :body (read-source-code first-part)))))))

(defun read-group-of-expressions (stream)
  (iterate
    (with paranthesis-depth = 0)
    (for next-char = (peek-next-char stream))

    (until (and (equal next-char #\))
                (= paranthesis-depth 1)))

    (cond
      ((equal next-char #\()
       (incf paranthesis-depth)
       (read-next-char stream)
       (next-iteration))

      ((equal next-char #\))
       (decf paranthesis-depth)
       (read-next-char stream)
       (next-iteration)))

    (collect (read-next-char stream) into result result-type 'string)

    (finally
     (read-next-char stream) ; read closing paranthesis
     (return (make-code-block :body (read-source-code result))))))
