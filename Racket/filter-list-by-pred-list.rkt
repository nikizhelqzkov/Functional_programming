#lang racket

(define (isOk? el pred-xs)
  (cond [(null? pred-xs)#t]
        [((car pred-xs)el)(isOk? el (cdr pred-xs))]
        [else #f]))

(define (where xs pred-xs)
  (filter (λ(x)(isOk? x pred-xs))xs))
(where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5))))
