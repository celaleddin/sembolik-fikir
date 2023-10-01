(define-generic-mode 'sf-mode
  '() ; comment
  '("sembol:")
  '(("^sf >$" 0 font-lock-builtin-face)

    ("'\\S-*" 0 'font-lock-type-face)

    ("\\s-\\(olsun\\)[.:]\\s-" 1 font-lock-keyword-face)
    ("\\s-\\(|\\)\\s-" 1 font-lock-builtin-face)

    ("\\b\\(\\S-+:\\)\\s-" 1 font-lock-function-name-face)
    ("\\s-\\([^]).'[:blank:]]+\\)\\s-*[].)]" 1 font-lock-function-name-face)

    ("\".*\"" 0 font-lock-string-face)
    ("[0-9]+" 0 font-lock-constant-face))
  '("\\.sf$")
  nil
  "A mode for sembolik-fikir files")

(add-hook 'sf-mode-hook #'rainbow-delimiters-mode)
