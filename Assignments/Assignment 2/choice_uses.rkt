#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require racket/include)
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 4
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#
(define (subsets lst)
  (let ([x (checked-subsets lst)])
    (feed-< x)))

#| HELPER FUNCTIONS
(append2all lst element)
  lst: a list (of lists)
  element: the element to append

  Adds the element to each list within list.

> (append2all '((1 2 3 4) (5 6 7)) 8)
'((8 1 2 3 4) (8 5 6 7))
|#
(define (append2all lst element)
  (map (λ(elem) (append (list element) elem))
       lst))

#|
(unchecked-subsets lst)
  lst: a list

  Gets all subsets of list without checking for duplications.

> (unchecked-subsets '(1 2 2))
'(() (2) (2) (2 2) (1) (1 2) (1 2) (1 2 2))
|#
(define (unchecked-subsets lst)
  (cond [(empty? lst) (list empty)]
        [else (let ([x (unchecked-subsets (rest lst))])
                (append x (append2all x (first lst))))]))

#|
(checked-subsets lst)
  lst: a list

  Gets all subsets of list, checking and removing duplicates.

> (checked-subsets '(1 2 2))
'(() (2) (2 2) (1) (1 2) (1 2 2))
|#
(define (checked-subsets lst)
  (remove-duplicates (unchecked-subsets lst)))

#|
(feed-< lsts)
  lsts: a list of lists

  Feeds -< to a list of lists.
|#
(define (feed-< lsts)
  (cond [(empty? lsts) empty]
        [(equal? (length lsts) 1) (first lsts)]
        [else (-< (first lsts) (feed-< (rest lsts)))]))

; QUESTION 5
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define (sudoku-4 puzzle)
  (let ([possibilities (feed-< (possible-sodukus puzzle))])
    (?- sudoku? possibilities)))

#|
Predicate used for correct sudoku puzzles, used in ?-
|#
(define (sudoku? puzzle)
  (valid-puzzle puzzle))


#|
Gets row (indexed 0, 1, 2, 3, going down) from puzzle
|#
(define (get-row row puzzle)
  (list-ref puzzle row))

#|
Gets column (indexed 0, 1, 2, 3, going left to right) from puzzle
|#
(define (get-column column puzzle)
  (map (λ(row)(list-ref row column))
       puzzle))

#|
Gets the soduku square (indexed 0, 1, 2, 3, going left to right, up to down) from puzzle
|#
(define (get-square square puzzle)
  (let ([flat-lst (flatten puzzle)])
    (cond [(equal? square 0) (list (list-ref flat-lst 0)
                                   (list-ref flat-lst 1)
                                   (list-ref flat-lst 4)
                                   (list-ref flat-lst 5))]
          [(equal? square 1) (list (list-ref flat-lst 2)
                                   (list-ref flat-lst 3)
                                   (list-ref flat-lst 6)
                                   (list-ref flat-lst 7))]
          [(equal? square 2) (list (list-ref flat-lst 8)
                                   (list-ref flat-lst 9)
                                   (list-ref flat-lst 12)
                                   (list-ref flat-lst 13))]
          [(equal? square 3) (list (list-ref flat-lst 10)
                                   (list-ref flat-lst 11)
                                   (list-ref flat-lst 14)
                                   (list-ref flat-lst 15))])))

#|
Returns whether or not the given soduku group is valid
|#
(define (valid-group group)
  (if (false? (check-duplicates group)) #t #f))

#|
Returns whether or not every element in bool-lst is true.
|#
(define (and-all bool-lst)
  (cond [(empty? bool-lst) #t]
        [else (and (first bool-lst)
                   (and-all (rest bool-lst)))]))

#|
Returns whether or not the given sudoku puzzle is valid
|#
(define (valid-puzzle puzzle)
  (let* ([indices '(0 1 2 3)]
         [rows (map (λ(index)(get-row index puzzle))
                    indices)]
         [cols (map (λ(index)(get-column index puzzle))
                    indices)]
         [sqrs (map (λ(index)(get-square index puzzle))
                    indices)]
         [groups (append rows cols sqrs)]
         [group-validity (map (λ(grp)(valid-group grp))
                              groups)])
    (and-all group-validity)))

#|
Returns whether or not the given group matches pattern.
A soduku group will match a pattern if it has the same
elements in the same spaces as the pattern.

> (group-match '(1 2 3 4) '(1 "" "" 2))
#f
> (group-match '(1 2 3 4) '(1 2 3 4))
#t
> (group-match '(1 2 3 4) '(1 "" "" ""))
#t
|#
(define (group-match group pattern)
  (let* ([pairs (make-pairs group pattern)]
         [matches (map (λ(pair)(elem-matches
                                (first pair)
                                (last pair)))
                       pairs)])
    (and-all matches)))


#|
Returns whether or not soduku matches pattern.
A soduku will match a pattern if it has the same
elements in the same spaces as the pattern.

> (define grid1
  '((1 2 3 4)
    ("" "" 1 "")
    ("" "" 2 3)
    (2 "" "" 1)))
> (define completed
    '((1 2 3 4)
        (3 4 1 2)
        (4 1 2 3)
        (2 3 4 1)))
> (soduku-match completed grid1)
#t

|#
(define (soduku-match soduku pattern)
  (let* ([row-pattern-pairs (make-pairs soduku pattern)]
         [row-matches (map (λ(pair)(group-match (first pair)
                                                (last pair)))
                           row-pattern-pairs)])
    (and-all row-matches)))

#|
Returns a list of lists in the following form:
(Every possible group configuration that matches pattern ...)

> (possible-groups '(1 2 3 4))
'((1 2 3 4))
> (possible-groups '(1 2 3 ""))
'((1 2 3 4))
> (possible-groups '(1 2 "" ""))
'((1 2 3 4) (1 2 4 3))

|#
(define (possible-groups pattern)
  (let ([all-permutations (permutations '(1 2 3 4))])
    (filter (λ(row)(group-match row pattern))
            all-permutations)))

#|
Returns a triple-nested list in the following form:
((Every possible first-row configuration that matches the first pattern ...)
 (Every possible second-row configuration that matches the second pattern ...)
 ...
 (Every possible fourth-row configuration that matches the fourth pattern ...)
)
|#
(define (row-possibilities soduku-pattern)
  (map (λ(row)(possible-groups row))
       soduku-pattern))

#|
Prepends item to all the lists in lst.
If there are no lists in lst, return empty
|#
(define (prepend-to-all item lst)
  (if (empty? lst)
      empty
      (cons (cons item
                  (first lst))
            (prepend-to-all item
                            (rest lst)))))

#|
Prepends each item from items to all the lists in lst
|#
(define (prepend-items-to-all items lst)
  (apply append
         (map (λ(item)(prepend-to-all item lst))
              items)))


#|
"Traverses" lst in the following fashion:
If lst = '((A B) (C D E))
Returns: '((A C) (A D) (A E) (B C) (B D) (B E))

> (traverse-nested '((1 2)(3 4)))
'((1 3) (1 4) (2 3) (2 4))
|#
(define (traverse-nested lst)
  (cond [(empty? lst) (list empty)]
        [else (let ([traversals (traverse-nested (rest lst))])
                (prepend-items-to-all (first lst)
                                      traversals))]))


#|
Returns every possible soduku board from soduku pattern.
These possibilities are dependent on possible rows and may
therefore not be valid configurations.
|#
(define (possible-sodukus soduku-pattern)
  (traverse-nested (row-possibilities soduku-pattern)))

#|
Returns the factorial of n.

> (factorial 0)
1
> (factorial 3)
6
|#
(define (factorial n)
  (if (equal? n 0)
      1
      (* n
         (factorial (- n 1)))))

#|
Makes a list of lists from same-length lists lst1 and lst2
in the form ((lst1[i] lst2[i]) ...) for every 0 <= i < (length lst1).
> (make-pairs '(1 2) '(3 4))
'((1 3) (2 4))
> (make-pairs '() '())
'()
|#
(define (make-pairs lst1 lst2)
  (cond [(empty? lst1) empty]
        [else (cons (list (first lst1)
                          (first lst2))
                    (make-pairs (rest lst1)
                                (rest lst2)))]))

#|
Returns true iff elem is equal to to-comp, or if to-comp is "".
> (elem-matches 1 "")
#t
> (elem-matches 1 1)
#t
> (elem-matches 1 2)
#f
|#
(define (elem-matches elem to-comp)
  (or (equal? "" to-comp)
      (equal? elem to-comp)))

; QUESTION 6
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <expr>)
     (foldl <combine> <init> (all <expr>))]))