#lang racket
(define ( myadv x)
  (define (helper d)
    (if (= 0(remainder x d ))
        d
        (helper (- d 1))))
  (helper (- x 1)))

(myadv 13)
        