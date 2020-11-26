#lang racket
(define (max-ordered-prefix xs)
  (cond [(null? xs)'()]
        [(or (null? (cdr xs))
             (>= (car xs)(cadr xs)))(list (car xs))]
        [else (cons (car xs)(max-ordered-prefix (cdr xs)))]))

(define (max-ordered-sublist xs)
  (define (helper xs max-sub)
    (define max-cur (max-ordered-prefix xs))
    (cond [(null? xs)max-sub]
          [(>(length  max-cur)(length  max-sub))(helper (drop xs (length  max-cur))max-cur)]
          [else (helper (drop xs (length  max-cur))max-sub)]
          ))
  (helper xs '()))

(max-ordered-sublist '(1 5 2 4 6 8 3 4 1)); →'(2 4 6 8)