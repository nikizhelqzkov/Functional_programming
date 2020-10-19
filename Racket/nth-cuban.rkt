#lang racket



(define (nth-cuban n)
  (define (prime? n)
    (define(helper d )
      (cond [(= d n)#t]
            [(= (remainder n d)0)#f]
            [else (helper (+ d 1))]))
       (if (= n 1)
           #f
           (helper 2)))
  (define (dif-cub m k)
    (- (expt m 3) (expt k 3)))
  (define (func f1 f2 i n)
    (cond [(= n 0)1]
          [(and(>= i n)(prime? (dif-cub f1 f2)))(dif-cub f1 f2)]
          [(not (prime?(dif-cub f1 f2))) (func (+ f1 1)(+ f2 1)i n)]
         ; [(func (+ f1 1) (+ f2 1) (+ i 1) n)]   
          [else (func (+ f1 1) (+ f2 1) (+ i 1) n)]
          )
    )
  (func 2 1 1 n)
  )

   (nth-cuban 100)      
           