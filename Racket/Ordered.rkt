#lang racket
(define (ordered? list pred)
  (cond[(null? (cdr list))#t]
       [(pred(car list)(car (cdr list)))
        (ordered? (cdr list) pred)]
       [else #f]))


(ordered? '(1 2 3 6 7) (λ (x y) (< x y))) 

    