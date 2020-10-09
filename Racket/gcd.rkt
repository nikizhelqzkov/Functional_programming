#lang racket
(define (gcd a b)
  (cond [(= a b)a]
        [(< a b) (gcd a (- b a))]
        [else (gcd (- a b)b)]))

(gcd 66 16)
        