#lang racket
(define (repeated f n)
  (define (helper f n x)
  (if (= n 0)
      x
      (f (helper f (- n 1) x))))
  (λ(x)(helper f x n)))

((repeated (λ(x) (* x 2))3)5)

