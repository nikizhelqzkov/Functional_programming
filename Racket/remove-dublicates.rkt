#lang racket
(define (remove-dublicates list)
  (if (null? list)
      '()
      (cons (car list)
            (remove-dublicates (filter (λ(x)(not (= x (car list))))(cdr list))))))

(remove-dublicates '(1 2 2 3 5 4 4 3 6 8))
