#lang racket
(define(sum-sum-digit a b k)
  
  (define (sum-d-n n res)
    (cond[(< n 10)(+ res n)]
         [else (sum-d-n (quotient n 10)(+ res (remainder n 10)))]))

  (define (isDevide? n k)
    (if (= 0(remainder (sum-d-n n 0) k))
        #t
        #f))
  (define (sum-range a res)
    (cond [(> a b)res]
          [(isDevide? a k)(sum-range (+ a 1) (+ a res))]
          [else (sum-range (+ a 1) res)]))
  (sum-range a 0))

(sum-sum-digit 100 110 2)