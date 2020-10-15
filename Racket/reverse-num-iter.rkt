#lang racket
;
  
(define (reverse-number-iter n)
  (define (helper s i)
    (if(< i 10)
    (+ (* s 10) i)
    (helper (+ (remainder i 10) (* s 10))(quotient i 10))))
  (helper 0 n))

(reverse-number-iter 152)
    ;1525 abc cba
     ;(0*10 + c)*10)+ b) * 10 + a