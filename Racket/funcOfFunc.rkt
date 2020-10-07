#lang racket
(define (average x y)(/ (+ x y ) 2))
(define (square x) (* x x))
(define (as x y)(*(average (square x) (square y)) 2))
(as 2 3)


