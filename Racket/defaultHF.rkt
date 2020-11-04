#lang racket
(define (myf f)
  (λ(x)(f x)))
((myf (λ(x)(* 3 x)))5)

  