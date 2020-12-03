#lang racket
(define(shuffle xs)
  (define len (length xs))
  (define (helper xs count l)
  (cond [(or (= count (quotient len 2))(null? xs))'()]
        [else (cons (list (car xs) (car (drop l (+ count(/ len 2))))) (helper (cdr xs) (+ count 1) l))]
        ))
 
   (helper xs 0 xs ))
(shuffle '(2 5 1 3 4 7))