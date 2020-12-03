#lang racket
(define(dig-pow n p)
  (define (count n)
    (cond [(< n 10) 1]
        [else (+ 1 (count (quotient n 10)))]
        ))
  (define (helper n sum count)
    (cond [(< n 10)(+ sum (expt n (+ count (- p 1))))]
          [else (helper (quotient n 10)(+ sum (expt (remainder n 10)(+ count (- p 1))))(- count 1))]))

  (if (= 0 (remainder (helper n 0 (count n)) n))
      (quotient (helper n 0 (count n)) n)
      -1
      ))

(dig-pow 89 1)
(dig-pow 92 1)
(dig-pow 695 2)
(dig-pow 46288 3)
