(in-package :sf/reader)

(defvar *extensions* '())

(defstruct extension variations integration-char)

(defun register-extension% (&rest args &key variations integration-char)
  (declare (ignore integration-char))
  (let* ((maybe-extension (find-extension-by-variations variations)))
    (or maybe-extension
        (first (push (apply #'make-extension args) *extensions*)))))

(defmacro register-extension (variations &optional integration-char)
  `(register-extension% :variations ',variations
                        :integration-char ,integration-char))

(defun extension-regex-string (extension)
  (with-slots (variations integration-char) extension
    (format nil "^\(~@[~A?~]\(~{~A~^\|~}\)\)\(-|$\)" integration-char variations)))

(defun find-extension-by-variations (variation-list)
  (find-if (lambda (extension)
             (some (lambda (variation)
                     (string= variation (first variation-list)))
                   (extension-variations extension)))
           *extensions*))

(defun find-extension-by-string (extension-string)
  (iter (for extension in *extensions*)
    (when (scan (extension-regex-string extension) extension-string)
      (return extension))))

(defun extension-canonical-form (extension-string)
  (let ((ext (find-extension-by-string extension-string)))
    (regex-replace (extension-regex-string ext)
                   extension-string
                   (list (first (extension-variations ext)) 2))))

(register-extension ("i" "ı" "ü" "u") "y")
(register-extension ("e" "a") "y")
(register-extension ("de" "da" "te" "ta"))
(register-extension ("den" "dan" "ten" "tan"))

(register-extension ("er" "ar") "ş")

(register-extension ("in" "ın" "ün" "un") "n")
