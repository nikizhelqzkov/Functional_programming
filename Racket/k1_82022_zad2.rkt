#lang racket
(define (kth-max-min xs)
   (define (negativeL xs)
      (filter (λ(x)(< x 0)) xs))
(define nList (sort (negativeL xs) >))
  (define (count nList c)
    (cond [(null? (cdr nList))(+ c 1)]
          [(>(car nList)(cadr nList))(count (cdr nList)(+ 1 c))]
          [else (count (cdr nList)c)]))

  (λ(x)(cond[(< (count nList 0)x)(error "No such number")]
       [else (car (drop nList (- x 1)))])))


((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0))2)
((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3)

   

