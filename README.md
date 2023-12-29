## Sembolik Fikir

Türkçe'ye daha yakın deneysel programlama dili

```
> (a'nın karesi) olsun: (
    a çarpı: a.
  ).

> 5'in karesi.
25

> 42'nin karesi.
1764

> rastgele-bir-sayı -25 olsun.

> rastgele-bir-sayı'nın karesi.
625

> doğru değil.
yanlış

> 7'den 70'e 6'şar listele.
(7 13 19 25 31 37 43 49 55 61 67)

> 3'ten 30'a 4'er listele.
(3 7 11 15 19 23 27)

> pi 3.14, yarıçap 5 olsun: (
    yazdır: "Çemberin alanı:".
    pi çarpı: (yarıçap'ın karesi).
  ).
Çemberin alanı:
78.5

> 5-ekleme [ sayı | sayı artı: 5 ] olsun.

> 5-ekleme'yi uygula: 10.
15

> (fonksiyon'un x'te değeri) olsun: (
    fonksiyon'u uygula: x.
  ).

> (f'nin türevi) olsun: (
    [ t |
      delta-x 1/100000
      delta-y ((f'nin (t artı: delta-x)'te değeri) eksi:
               (f'nin t'de değeri)) olsun: (
        delta-y bölü: delta-x.
      ).
    ]
  ).

> kare-fonksiyonu [ x | x çarpı: x ] olsun.

> kare-fonksiyonu'nun 10'da değeri.
100

> (kare-fonksiyonu'nun türevi)'nin 10'da değeri.
20.00001
```
