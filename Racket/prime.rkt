#lang racket
(define (prime? n)
(define(helper d )
  (cond [(= d n)#t]
        [(= (remainder n d)0)#f]
        [else (helper (+ d 1))]))
  (if (= n 1)
      #f
      (helper 2)))

(prime? 47)
  