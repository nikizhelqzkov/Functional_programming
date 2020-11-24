#lang racket
(display "EXERCISE 1 TESTS: \n")
(define (cartesian-product xs ys)
(define (helper xs y2)
  (cond [(null? xs)'()]
        [(null? y2)(helper (cdr xs) ys)]
        [else (cons (cons (car xs)(car y2))(helper xs (cdr y2)))]
        ))
  (helper xs ys))
(cartesian-product '(1 2) '(3 4))
(cartesian-product '(1 2 3 4 5) '(6 7 8))

