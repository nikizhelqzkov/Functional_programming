#lang racket
(define(repeater str)

  (define (rep str count res exp)
  (cond [(= count 0) res]
  [else (rep str (- count 1)(string-append res ((λ(s) (string-append str s))exp)) exp)]))
  
  (λ(count guet)(rep str count "" guet)))

((repeater "I love Racket") 3 " ")
((repeater "Quack") 5 "!")