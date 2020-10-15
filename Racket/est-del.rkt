#lang racket
;25  dali 25 e , dali 24 e ... dali 1 e

(define (delitel? n d)
  (= 0(remainder n d)))

(define (count-del n d count)
(cond [(< d 1)count]
    [(delitel? n d)(count-del n (- d 1)(+ count 1))]
    [else (count-del n (- d 1) count)]))

(define (del n)
  (count-del n n 0))

