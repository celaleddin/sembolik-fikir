(in-package :sf/tests)

(def-suite akış
  :description "SF akış sözdizimi testleri"
  :in sf-tests)

(in-suite akış)

(test kere-yap
  (is
   (= (eval-sf "
        a 0 olsun.
        10 kere-yap: [
          a =: (a artı: 1).
        ].
        a.")
      10)))
