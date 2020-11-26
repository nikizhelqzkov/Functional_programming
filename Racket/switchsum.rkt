#lang racket


;(define n 5)

  (define (switchsum f g n)
     (λ(x)
    (define (sumOfFuncs f g n)
  (cond [(= n 0)x]
        [(= (remainder n 2)1)(f(sumOfFuncs f g (- n 1)))]
        [else (g(sumOfFuncs f g (- n 1)))]))
   
  (define (helper count sum)
    (cond [(> count n)sum]
          [else (helper (+ count 1 )(+ sum (sumOfFuncs f g count) ))]))
  (helper 1 0)))


((switchsum (lambda (x) (+ x 1)) (lambda (x) (* x 2)) 2)2)
