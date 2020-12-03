#lang racket
; '() - празен списък
; (null? ...) - проверява дали списък е празен
; (cons ... ...) - създава точкова двойка
; (car ...) - начало на списък / първи на точк. дв.
; (cdr ...) - опашка на списък / втори на точк. дв.

(define (size lst)
(if (null? lst)
0
(+ 1 (size (cdr lst)))))




(define (isHas n list)
  (cond[ (null? list)#f]
       [(= (car list) n)#t]
       [(null? (cdr list))#f]
       [else (isHas n (cdr list))]))

(isHas 7 (cons 3 (cons 5 (cons 5 '()))))



(define (insert-at lst x pos)
(if (= pos 0)
(cons x lst)
(cons (car lst) (insert-at (cdr lst) x (- pos 1)))))


(define (find-min list)
(define (minNList num list)
(cond [(null? list)num]
      [(> num (car list))(minNList (car list)(cdr list))]
      [else (minNList num (cdr list))]))
(minNList (car list)list))
(find-min '(1 2 3 -3 8 6))


(define (erase num list)
  (cond [(null? list)list]
        [(= num (car list))(cdr list)]
        [else (cons (car list)(erase num (cdr list)))]))

(erase 3 '(1 3 3 4 5))



(define (allErase num list)
  (cond [(null? list)list]
        [(= num (car list))(allErase num (cdr list))]
        [else (cons (car list)(allErase num (cdr list)))]))
(allErase 3 '(1 3 3 4 5))




(define (concat list1 list2)
  (if (null? list1)
      list2
   (cons (car list1) (concat (cdr list1) list2))))

(concat '(1 3 3 4 5) '(1 6 5 4 7))


(define (reverse list)
  (define (helper list res)
    (if( null? list)
       res
       (helper (cdr list)(cons (car list)res))))
    (helper list '()))

(reverse '(1 3 2))





  