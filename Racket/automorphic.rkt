#lang racket
(define (automorphic? n)
  (define (helper sq n )
    (if (>= n 10); proverqvame dali n e s poveche ot edna cifra. Imitaciq na while cikul.
        (if (not (= (remainder n 10)(remainder sq 10))) ;ako n ima poveche ot 1 cifra proverqvame dali poslednata cifra na sq i n e razlichna
            #f ; ako e razlichna krai na zadachata s false
            (helper (quotient sq 10) (quotient n 10))); ako sa ednakvi togava delim sq i n celochicleno

        (if(= (remainder n 10)(remainder sq 10));  ako e samo s 1 cifra n , togava proverqvame dali poslednite cifri na sq i n sa ednakvi
           #t ; ako sa true
           #f ; inache false
           )))
  (helper (sqr n) n )) ; sq promenlivata q pravim s vgradena funkciq da e kvadrata na n

(automorphic? 125)