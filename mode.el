(define-generic-mode 'sf-mode
  '() ; comment
  '()
  '(("^sf-file >$" 0 font-lock-builtin-face)

    ("[0-9]+" 0 font-lock-constant-face)
    ("\".*\"" 0 font-lock-string-face)

    ("\\s-\\(olsun\\)[.:]\\s-*" 1 font-lock-keyword-face)

    ("\\b\\(\\S-+:\\)\\s-" 1 font-lock-function-name-face)
    ("[^.:[(]\\s-+\\([^]).[:blank:]]+\\)\\s-*[].)]" 1 font-lock-function-name-face)

    ("'\\(\\S-+\\)" 1 'font-lock-type-face))
  '("\\.sf$")
  nil
  "A mode for sembolik-fikir files")

(add-hook 'sf-mode-hook #'rainbow-delimiters-mode)
