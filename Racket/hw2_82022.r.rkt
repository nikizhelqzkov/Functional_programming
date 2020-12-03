#lang racket
(display "EXERCISE 1 TESTS: \n")
(define (cartesian-product xs ys)
(define (helper xs y2)
  (cond [(null? xs)'()]
        [(null? y2)(helper (cdr xs) ys)]
        [else (cons (cons (car xs)(car y2))(helper xs (cdr y2)))]
        ))
  (helper xs ys))
(cartesian-product '(1 2) '(3 4))
(cartesian-product '(1 2 3 4 5) '(6 7 8))

(display "\n EXERCISE 2 TESTS: \n")
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
