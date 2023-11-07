(in-package :sf/tests)

(def-suite transform
  :description "SF -> LISP transformation tests"
  :in sf-tests)

(in-suite transform)

(test simple-variable-definition
  (is-all-equal
    ((sf "a 5 olsun.") '(defparameter |a| 5))
    ((sf "sayı olsun: 20.") '(defparameter |sayı| 20))))

(test comments
  (is-all-equal
    ((sf ";; bir yorum") nil)
    ((sf "10. ;; satır içi yorum") 10)))

(test numbers
  (is-all-equal
    ((sf "42") 42)
    ((sf "15.") 15)
    ((sf "3.1415") 3.1415)))

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
  (is-all-equal
    ((sf "(a'nın karesi) olsun: ( a a @* )")
     '(defun |karesi/in| (|a|)
       (* |a| |a|)))

    ((sf "(yazdır: şey) olsun: ( şey @print )")
     '(defun |yazdır:| (|şey|)
       (print |şey|)))

    ((sf "(x'ten y'ye z'şer listele) olsun: (
            @(iterate
              (for k :from |x| :to |y| :by |z|)
              (print k))
          )")
     '(defun |listele/den/e/er| (|x| |y| |z|)
       (iterate
         (for k :from |x| :to |y| :by |z|)
         (print k))))

    ((sf "(x'ten y'ye yap: prosedür) olsun: (
            @(mapcar |prosedür| (list |x| |y|))
          )")
     '(defun |yap:/den/e| (|prosedür| |x| |y|)
       (mapcar |prosedür| (list |x| |y|))))
    ))

(test procedure-call
  (is-all-equal
    ((sf "5'in karesi") '(|karesi/in| 5))
    ((sf "3'ün karesi") '(|karesi/in| 3))
    ((sf "2'nin karesi") '(|karesi/in| 2))
    ((sf "6'nın karesi") '(|karesi/in| 6))
    ((sf "10'un karesi") '(|karesi/in| 10))

    ((sf "yazdır: \"bir metin\"") '(|yazdır:| "bir metin"))
    ((sf "yazdır: değişken") '(|yazdır:| |değişken|))
    ((sf "yazdır: ( 1. 2. 3. )") '(|yazdır:| (progn 1 2 3)))

    ((sf "3'ten 120'ye 6'şar listele") '(|listele/den/e/er| 3 120 6))
    ((sf "6'dan 49'a 3'er listele") '(|listele/den/e/er| 6 49 3))

    ((sf "1'den z'ye yap: prosedür") '(|yap:/den/e| |prosedür| 1 |z|))
    ))

(test code-block
  (is-all-equal
   ((sf "yazdır: ( bir-sayı bir-başka-sayı topla. 10 )")
    '(|yazdır:| (progn (|topla//| |bir-sayı| |bir-başka-sayı|)
                       10)))
    ))
