#lang racket
(define (dif F a b)
  (- (F b) (F a)))
(dif (λ(x)(+ x 2))3 5)

