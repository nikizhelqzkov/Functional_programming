#lang racket
(define (triangular? xs)
  (define (helper xs l count curC)
  (cond [(null? l)#t]
        [(list? (car xs))(helper (car xs) l count curC)]
        [(and (not(= (car xs)0)) (< curC count))#f]
        [(or (>= curC count)(null? xs))(helper (cdr l)(cdr l)(+ 1 count)1)]
        [else (helper (cdr xs)l count (+ 1 curC))]
        ))
  (helper xs xs 1 1)
  )
  (triangular? '((1 2 3)(0 5 6)(0 0 9)))
(triangular? '((0 2 3)(0 0 6)(1 0 0)))
(triangular? '((1 2 3)(1 5 6)(0 0 9)))
(triangular? '((1 2 3 4)(0 5 6 7)(0 0 8 9)(0 0 0 9)))
