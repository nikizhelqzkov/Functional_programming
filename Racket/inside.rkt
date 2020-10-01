#lang racket
(define (inside x a b)
  (if (and (>= x a)(<= x b))
      #t
      #f))
(inside 4 1 6)
(inside 9 1 6)