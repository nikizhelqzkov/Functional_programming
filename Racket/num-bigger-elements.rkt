#lang racket
(define (num-bigger-elements lst)
  (define (count el lst res)
    (cond [(null? lst)res]
          [(< el (car lst))(count el (cdr lst)(+ 1 res))]
          [else (count el (cdr lst)res)]))
  (define (helper l )
    (cond [(null? l)'()]
          [else (cons (list (car l)(count (car l)lst 0)) (helper (cdr l)))]))
  (helper lst))

(num-bigger-elements '(5 6 3 4))
