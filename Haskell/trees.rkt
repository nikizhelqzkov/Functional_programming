#lang racket
;(define tree '((a b c)
 ;              (b d e)
;               (c f g h)
;               (g i j)))

;(define (succs? node)
 ; (let ([lst (assq node tree)])
 ;   (if lst (cdr lst) '()) 
 ;   ))


;(define (leaf? node)
;  (null?(succs? node))
;  )


(define tree '(1
               (2
                (3))
               (4
                (5
                 (6)))
               (7
                (8)
                (9
                 (10
                  (11))))))

(define treeTwig' (1
                    (2)
                    (3)
                    (4)
                    ))

(define treeTwig2' (1
                    (2)
                    
                    ))

(define (leaf? tree)
  (null? (cdr tree))
  )

(define (twig? tree)
  (andmap leaf? (cdr tree))
  )


(define (stick t)
(or (leaf? t) (and (null? (cdr(cdr t))) (stick (car (cdr t))))))

(define (trim t)
  (cons (car t) (map trim (filter (λ (x) (not (twig? x))) (cdr t))))
  )


(define (prune t)
  (if (leaf? t) t
   (cons (car t) (if (stick t)
                    (list (list (caadr t)))
                    (map prune (cdr t))))
         ))
  
  
