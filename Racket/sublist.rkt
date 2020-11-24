#lang racket
(define (sublist start end list)
  (take (drop list start)(+ 1 (- end start))))



(sublist 3 6 '(1 2 3 4 5 6 7 8 9 10))              