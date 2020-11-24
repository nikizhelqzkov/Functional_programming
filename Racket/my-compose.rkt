#lang racket
(define (my-compose f g)
  (λ(x)(f(g x))))
((my-compose (λ (x)(+ x 2))(λ(x) (* x 2)))3)
