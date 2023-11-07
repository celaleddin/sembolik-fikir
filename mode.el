(define-generic-mode 'sf-mode
  '(";")
  '()
  '(("^sf >$" 0 font-lock-builtin-face)
    ("," 0 'font-lock-comment-face)

    ("'\\S-*" 0 'font-lock-type-face)

    ("\\s-\\(sözdizimi-olsun:\\)\\s-" 1 font-lock-keyword-face)
    ("\\s-\\(olsun\\)[.:]" 1 font-lock-keyword-face)

    ("\\s-\\(|\\)\\s-" 1 font-lock-builtin-face)
    ("\\s-\\(@[(`]\\)" 1 font-lock-builtin-face)

    ("\\b\\(\\S-+:\\)\\s-" 1 font-lock-function-name-face)
    ("[^:]\\s-+\\([^]).'[:blank:]]+\\)\\s-*[].)]" 1 font-lock-function-name-face)

    ("\".*\"" 0 font-lock-string-face)
    ("\\s-\\([0-9]+\\)\\b" 1 font-lock-constant-face))
  '("\\.sf$")
  (list
   (lambda ()
     (setq-local tab-width 2)
     (setq-local indent-tabs-mode nil)
     (setq-local indent-line-function 'simple-indent-line)))
  "A mode for sembolik-fikir files")

(add-hook 'sf-mode-hook (lambda () (rainbow-delimiters-mode 1)))
(add-hook 'sf-mode-hook (lambda () (smartparens-mode 1)))

;; Below indentation code is based on:
;; https://gist.github.com/eddieh/33aaa48625001992d8584bfdd5008501
(defun simple-indent-line ()
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (simple-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun simple-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (* tab-width (min (car (syntax-ppss (line-beginning-position)))
                    (car (syntax-ppss (line-end-position))))))
