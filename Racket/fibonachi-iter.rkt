#lang racket
(define (fib-iter n)
  (helper 1 1 1 n))
(define(helper prev cur i n)
  (if (>= i n)
      cur
      (helper cur (+ prev cur) (+ i 1) n)))

  (fib-iter 25150)
  