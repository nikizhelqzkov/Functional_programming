#lang racket
(define (square x)(* x x))
(define (avr x y)(/ (+(square x)(square y))2))
(avr 3 5)