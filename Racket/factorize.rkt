#lang racket
(define (factorize n)
  (define (helper el n)
    
  (cond [ (> el n)'()]
        [(= 0(remainder n el))(cons el (helper el (quotient n el)))]
        [else (helper (+ 1 el) n)]))
  (helper 2 n))

(factorize 6)   ; -> '(2 3)
(factorize 13)  ; -> '(13)
(factorize 123) ; -> '(3 41)
(factorize 152) ; -> '(2 2 2 19)