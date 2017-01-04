#lang racket
(define (rec-reverse lst)
  (cond [(empty? lst) empty]
        [else (append (rec-reverse (rest lst))
                      (list (first lst)))]))

(define (tail-reverse lst)
  (cond [(< (length lst) 2) lst]
        [else (cons (last lst)
                    (tail-reverse (take lst (- (length lst) 1))))]))