(in-package :sf/tests)

(def-suite transform
  :description "SF -> LISP transformation tests"
  :in sf-tests)

(in-suite transform)

(test simple-variable-definition
  (is (subexpr?
       '(defparameter |a| 5)
       "a 5 olsun."))
  (is (subexpr?
       '(defparameter |sayı| 20)
       "sayı olsun: 20."))
  (is (subexpr?
       '(defparameter |b| 10)
       "b =: 10"))
  )

(test comments
  (is-all-equal
    ((sf ";; bir yorum") nil)
    ((sf "10. ;; satır içi yorum") 10)
    ((sf "10, 20, 30 @+") '(+ 10 20 30))
    ))

(test numbers
  (is-all-equal
    ((sf "42") 42)
    ((sf "15.") 15)
    ((sf "3.1415") 3.1415)
    ((sf "-32") -32)
    ((sf "1e10") 1e10)
    ((sf "2/30") 2/30)
    ))

(test let
  (is-all-equal
    ((sf "x 1, y 2, z 3 olsun: ( x y z @+ )")
     '(let* ((|x| 1) (|y| 2) (|z| 3))
       (+ |x| |y| |z|)))))

(test let-over-lambda
  (is-all-equal
    ((sf "x 1, y 2, z 3 olsun: [ x y z @+ ]")
     '(let* ((|x| 1) (|y| 2) (|z| 3))
       (lambda ()
         (+ |x| |y| |z|))))

    ((sf "x 1 y 2 z 3 olsun: [ t | x y z t @+ ]")
     '(let* ((|x| 1) (|y| 2) (|z| 3))
       (lambda (|t|)
         (+ |x| |y| |z| |t|))))))

(test procedure-definition
  (is (subexpr?
       '(defun |karesi'in| (|a|)
         (* |a| |a|))
       "(a'nın karesi) olsun: ( a a @* )"))
  (is (subexpr?
       '(defun |yazdır:| (|şey|)
         (print |şey|))
       "(yazdır: şey) olsun: ( şey @print )"))
  (is (subexpr?
       '(defun |listele'den'e'er| (|x| |y| |z|)
         (iterate
           (for k :from |x| :to |y| :by |z|)
           (print k)))
       "(x'ten y'ye z'şer listele) olsun: (
          @(iterate
            (for k :from |x| :to |y| :by |z|)
            (print k))
        )"))
  (is (subexpr?
       '(defun |yap:'den'e| (|prosedür| |x| |y|)
         (mapcar |prosedür| (list |x| |y|)))
       "(x'ten y'ye yap: prosedür) olsun: (
          @(mapcar |prosedür| (list |x| |y|))
        )"))
  )

(test procedure-call
  (is-all-equal
    ((sf "5'in karesi") '(|karesi'in| 5))
    ((sf "3'ün karesi") '(|karesi'in| 3))
    ((sf "2'nin karesi") '(|karesi'in| 2))
    ((sf "6'nın karesi") '(|karesi'in| 6))
    ((sf "10'un karesi") '(|karesi'in| 10))

    ((sf "yazdır: \"bir metin\"") '(|yazdır:| "bir metin"))
    ((sf "yazdır: değişken") '(|yazdır:| |değişken|))
    ((sf "yazdır: ( 1. 2. 3. )") '(|yazdır:| (progn 1 2 3)))

    ((sf "3'ten 120'ye 6'şar listele") '(|listele'den'e'er| 3 120 6))
    ((sf "6'dan 49'a 3'er listele") '(|listele'den'e'er| 6 49 3))

    ((sf "1'den z'ye yap: prosedür") '(|yap:'den'e| |prosedür| 1 |z|))
    ))

(test code-block
  (is-all-equal
   ((sf "yazdır: ( bir-sayı bir-başka-sayı @cl:+. 10 )")
    '(|yazdır:| (progn (cl:+ |bir-sayı| |bir-başka-sayı|)
                       10)))
    ))

(test list
  (is-all-equal
    ((sf "{ 1 2 3 }") '(list 1 2 3))
    ((sf "{ 1 (2 3 @+) 4 }") '(list 1 (+ 2 3) 4))
    ((sf "{ [ x | x'in karesi ] (2 3 @+) @'a-symbol }")
     '(list (lambda (|x|) (|karesi'in| |x|)) (+ 2 3) 'a-symbol))
    ))
