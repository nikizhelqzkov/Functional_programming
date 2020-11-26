#lang racket
(define (set-union xs ys)
    (cond [(and (null? xs)(null? ys))'()]
          [(and (null? xs)(not (null? ys)))(cons (car ys)(set-union xs (cdr ys)))]
          [(and (null? ys)(not (null? xs))) (cons (car xs)(set-union (cdr xs)ys))]
          [(= (car xs)(car ys)) (cons (car xs)(set-union (cdr xs)(cdr ys)))]
          [(< (car xs) (car ys))(cons (car xs)(set-union (cdr xs)ys))]
          [else (cons (car ys)(set-union xs (cdr ys)))]
          ))

(set-union '(1 3 5 7) '(5 7 13)) ;→'(1 3 5 7 13)
(set-union '(5 7 13) '(1 3 5 7)); → '(1 3 5 7 13)
    