#lang racket
(define (num-evens-higher-order lst)
  (length (filter even? lst)))
(num-evens-higher-order '(1 2 3 4))

(define (num-evens-recursive lst)
  (cond [(empty? lst) 0]
        [else (+ (cond [(even? (first lst)) 1]
                       [else 0])
                 (num-evens-recursive (rest lst)))]))
(num-evens-recursive '(1 2 3 4))

(define (add-to-all-higher-order lst element)
  (map (λ(cur-element)(append (list element) cur-element))
       lst))
(add-to-all-higher-order '((1 2 3 4) (4 5 6)) 1)

(define (add-to-all-recursive lst element)
  (cond [(empty? lst) empty]
        [else (cons (append (list element) (first lst))
                    (add-to-all-recursive (rest lst) element))]))
(add-to-all-recursive '((1 2 3 4) (4 5 6)) 1)

(define (subsets lst)
  (cond [(empty? lst) (list empty)]
        [else (let ([subsets-of-rest (subsets (rest lst))])
                (append
                       subsets-of-rest
                       (add-to-all-higher-order subsets-of-rest
                                                (first lst))))]))
(subsets '(1 2 3))

(define (bounded-subsets lst [size-bound -1])
  (cond [(equal? size-bound -1)
        (cond [(empty? lst) (list empty)]
               [else (let ([subsets-of-rest (bounded-subsets (rest lst))])
                (append
                       subsets-of-rest
                       (add-to-all-higher-order subsets-of-rest
                                                (first lst))))])]
        [else (filter (λ(subset) (< (length subset) (+ size-bound 1)))
                      (bounded-subsets lst -1))]))
(bounded-subsets '(1 2 3) 2)