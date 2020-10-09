#lang racket
(define(sum-odds a b)
  (cond [(> a b) 0]
        [(= (remainder a 2) 1)(+ a (sum-odds (+ a 2) b))]
        [else (sum-odds (+ a 1) b)]))

(sum-odds 1 1000)

(define (sum-odds-iter a b)
  (define (helper sum a)
    (if (> a b)
        sum
        (helper(+ sum a) (+ a 2))))
    (if (= (remainder a 2)1)
        (helper 0 a)
        (helper 0 (+ a 1))))

(sum-odds-iter 1 1000)