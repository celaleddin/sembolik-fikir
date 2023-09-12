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
          (make-phrase :base :|kelime|))

    (test "kelime'nin"
          (make-phrase :base :|kelime|
                       :extension "nin"))

    (test "1'in"
          (make-phrase :base 1
                       :extension "in"))

    (test "\"tırnak içi\"'nden"
          (make-phrase :base "tırnak içi"
                       :extension "nden"))

    (test "#(1 2 3)"
          (make-phrase :base #(1 2 3)))

    (test "#(1 2 3)'nden"
          (make-phrase :base #(1 2 3)
                       :extension "nden"))

    (test "()'in"
          (make-phrase :base (make-code-block)
                       :extension "in"))

    (test "[]'in"
          (make-phrase :base (make-procedure)
                       :extension "in"))
    ))

(defun test-expression ()
  (flet ((test (&rest rest)
           (apply (make-assert-func #'(lambda (expr-list)
                                        (car expr-list)))
                  rest)))
    (test "ankara'dan istanbul'a git"
          (make-expression :phrases (list
                                     (make-phrase :base :|ankara|
                                                  :extension "dan")
                                     (make-phrase :base :|istanbul|
                                                  :extension "a"))
                           :action (make-action :name "git")))

    (test "1'den 5'e-kadar yap: []"
          (make-expression :phrases (list
                                     (make-phrase :base 1
                                                  :extension "den")
                                     (make-phrase :base 5
                                                  :extension "e-kadar"))
                           :action (make-action :name "yap"
                                                :parameter (make-phrase :base (make-procedure)))))

    (test "1 2 3 4 5 topla"
          (make-expression :phrases (mapcar (lambda (i)
                                              (make-phrase :base i))
                                            '(1 2 3 4 5))
                           :action (make-action :name "topla")))
    ))

(defun run-reader-tests ()
  (test-phrase)
  (test-expression))
