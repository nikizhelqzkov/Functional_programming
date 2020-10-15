#lang racket
(define (reverse-number-iter n)
  (define (helper s i)
    (if(< i 10)
    (+ (* s 10) i)
    (helper (+ (remainder i 10) (* s 10))(quotient i 10))))
  (helper 0 n))

(define (palindrome? n)
  (= n (reverse-number-iter n)))
(define (palindrom-c a b)
  (cond [(> a b) 0]
        [(palindrome? a) (+ 1 ( palindrom-c (+ a 1) b))]
         [ else (palindrom-c (+ a 1) b)]))

(palindrom-c 1 150)
    
  

  ; 100 101