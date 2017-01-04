#lang racket
(define-syntax my-or
  (syntax-rules ()
    [(my-or p q)
     (if p #t q)]))

(my-or (> 10 3) (/ 1 0))


; Write a macro for "my-and"
(define-syntax my-and
  (syntax-rules ()
    [(my-and p q)
     (cond [(not p) #f]
           [else q])]
    [(my-and p q ...)
     (cond [(not p) #f]
           [else (my-and q ...)])]))
(my-and (> 10 3) (> 1 0))
(my-and (> 10 3) (> 1 0) (< 2 3))
(my-and (> 10 3) (> 1 0) (> 2 3))


; Recursive macro for "or"
(define-syntax my-or-rec
  (syntax-rules ()
    [(my-or-rec p) p]
    [(my-or-rec p q ...)
     (if p #t (my-or-rec q ...))]))