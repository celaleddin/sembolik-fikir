(in-package #:sf/tests)

(defun make-assert-func (expression-list-key)
  (lambda (source-code-string expected-structure)
    (assert (equalp (funcall expression-list-key (read-source-code source-code-string))
                    expected-structure))))

(defun test-phrase ()
  (flet ((test (&rest rest)
           (apply (make-assert-func #'(lambda (expr-list)
                                        (car (expression-phrases (car expr-list)))))
                  rest)))
    (test "kelime"
          (make-phrase :word "kelime"))

    (test "kelime'nin"
          (make-phrase :word "kelime"
                       :extension "nin"))

    (test "1'in"
          (make-phrase :word 1
                       :extension "in"
                       :is-word-lisp-object? t))

    (test "\"tırnak içi\"'nden"
          (make-phrase :word "tırnak içi"
                       :extension "nden"
                       :is-word-lisp-object? t))

    (test "#(1 2 3)"
          (make-phrase :word #(1 2 3)
                       :is-word-lisp-object? t))

    (test "#(1 2 3)'nden"
          (make-phrase :word #(1 2 3)
                       :extension "nden"
                       :is-word-lisp-object? t))
    ))

(defun test-expression ()
  (flet ((test (&rest rest)
           (apply (make-assert-func #'(lambda (expr-list)
                                        (car expr-list)))
                  rest)))
    (test "ankara'dan istanbul'a git"
          (make-expression :phrases (list
                                     (make-phrase :word "ankara"
                                                  :extension "dan")
                                     (make-phrase :word "istanbul"
                                                  :extension "a"))
                           :action (make-action :name "git")))

    (test "1'den 5'e-kadar yap: prosedür"
          (make-expression :phrases (list
                                     (make-phrase :word 1
                                                  :extension "den"
                                                  :is-word-lisp-object? t)
                                     (make-phrase :word 5
                                                  :extension "e-kadar"
                                                  :is-word-lisp-object? t))
                           :action (make-action :name "yap"
                                                :parameter (make-phrase :word "prosedür"))))

    (test "1 2 3 4 5 topla"
          (make-expression :phrases (mapcar (lambda (i)
                                              (make-phrase :word i
                                                           :is-word-lisp-object? t))
                                            '(1 2 3 4 5))
                           :action (make-action :name "topla")))
    ))

(defun run-reader-tests ()
  (test-phrase)
  (test-expression))
