#lang racket
(define (derive f eps)
  (λ(x)(/ (- (f (+ x eps))(f x))eps)))

(define (derive-2 f eps)
  (derive(derive f eps)eps))

(define (f1 x)
  (* 2 x x))
  (define f1-d
    (derive-2 f1 0.0001))

(f1-d 3)


(define (derive-n f n eps)
  (if (<= n 1)
      (derive f eps)
  (derive-n (derive f eps) (- n 1)eps)))

(define f1-n
  (derive-n f1 5 0.0001))

(f1-n 3)




(define (myCompose f g)
  (λ (x) (f (g x))))

(define (addOne x)(+ x 1))
(define (addTwo x)(+ x 2))

((myCompose addOne addTwo) 3)

(define (repeated f n)
  (λ(x) 
    (if (= n 0)
      x
      (f((repeated f (- n 1))x)))))
(define fd-5
  (repeated addOne 5))
(fd-5 1)




