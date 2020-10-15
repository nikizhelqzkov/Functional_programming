#lang racket
; n = sum(count del n)
; 2 2 1
(define (delitel? n d)
  (= 0(remainder n d)))
; 6 = 2 3 1
(define (sumN n s d) 
   (cond [(< d 1)s]
         [(delitel? n d)(sumN n (+ s d)(- d 1))]
         [else (sumN n s(- d 1))]))
(define (perfect? n)
   (= n (sumN n 0 (- n 1))))
(perfect? 6) 