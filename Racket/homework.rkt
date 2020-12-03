#lang racket
;ex.1
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

(automorphic? 890625)



; ex.2
(define (nth-cuban n) 
  (define (prime? n) ; algoritum - dali e prosto chislo
    (define(helper d )
      (cond [(= d n)#t]
            [(= (remainder n d)0)#f]
            [else (helper (+ d 1))]))
       (if (= n 1)
           #f
           (helper 2)))
  (define (dif-cub m k) ; pravim pomoshna funkciq za nth-cuban, koqto smqta razlikata na 2 chicla vsqko na treta
    (- (expt m 3) (expt k 3)))
  (define (func f1 f2 i n) ; pomoshna funkciq namirashta tova chislo
    (cond [(= n 0)1]
          [(and(>= i n)(prime? (dif-cub f1 f2)))(dif-cub f1 f2)] ; proverqvame dali e prosto zashtoto ako ne e shte vdignem s edno elementite
          [(not (prime?(dif-cub f1 f2))) (func (+ f1 1)(+ f2 1)i n)] 
          [else (func (+ f1 1) (+ f2 1) (+ i 1) n)]
          )
    )
  (func 2 1 1 n)
  )

   (nth-cuban 100)      