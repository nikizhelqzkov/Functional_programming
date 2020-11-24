#lang racket
(define (count-oc subl l)
  (define (helper list count-sub)
    (cond [(> count-sub (length list))0]
          [(equal? subl (take list count-sub))
           (+ 1 (helper (cdr list) count-sub))]
          [else (helper (cdr list) count-sub)]))

    (helper l (length  subl)))


  (count-oc '(1 2) '(1 2 2 1 2))

