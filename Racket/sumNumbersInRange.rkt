#lang racket
(define (sum-numbers a b)
  (define (isOrderedLow n)
  (cond[(< n 10) #t]
       [(>= (remainder (quotient n 10)10)(remainder n 10))(isOrderedLow (quotient n 10))]
       [else #f]))
  (define (helper a sum)
    (cond [(> a b)sum]
          [(isOrderedLow a)(helper (+ 1 a)(+ sum a))]
          [else (helper (+ 1 a)sum)]
          ))
  (helper a 0)
  )

(sum-numbers 1 9); →45
(sum-numbers 199 203); →200
(sum-numbers 219 225) ;→663
