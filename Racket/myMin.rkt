#lang racket
(define (mymin x y)
  (if (< x y)
      x
      y))

(mymin 75 9)