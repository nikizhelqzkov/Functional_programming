#lang racket
;(/ (- (f (+ x eps)) (f x)) eps)
(define (my-derive f eps)
  (λ(x) (/ (- (f (+ x eps))(f x))eps)))

(define (f1 x)
  (* 2 x x))

(define myF
  (my-derive f1 0.001))
(myF 2)


(define (derive2 f eps)
  (my-derive (my-derive f eps)eps))

(define myF2
  (derive2 f1 0.001))
(myF2 2)

(define (derive-n f n eps)
  (if (= n 0)
     f
     (my-derive (derive-n f (- n 1) eps)eps)))

(define myFn
  (derive-n f1 2 0.001))
(myFn 2)
           