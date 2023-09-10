(ql:quickload '(:iterate :split-sequence))
(use-package '(:iterate :split-sequence))


(defparameter *some-source-code* "
x 10 olsun.
(a'nın karesi) olsun: ( a çarpı: a. ).

5'in karesi.
1'den x'e-kadar listele.

toplam 0 olsun.
1'den x'e-kadar yap: [ i |
  toplam'a ekle: i.
].

a 3
b (3 çarpı: 3)
c [ 3 çarpı 5 ] olsun: (
  a ekle: b.
).
")
