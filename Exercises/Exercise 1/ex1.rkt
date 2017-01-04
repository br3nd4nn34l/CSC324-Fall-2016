#| Exercise 1 - Getting started with Racket (due Sept 24, 11:50pm on Markus)

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Racket libraries, unless explicitly told to.
- You may not use mutation or any iterative constructs (for/*).
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

Implement the three functions below to get some experience programming in Racket.
You may use either explicit recursion, or higher-order list functions.
(For extra practice, try both!)
|#
#lang racket

; This line exports the required functions. Don't change it!
(provide search-table search-table-2 max-sublist)

#|
(contains item lst)
   item: an object
   lst: a list of objects the same type as item

   Returns a boolean value indicating whether or not item is in lst.

> (contains 1 '(1 2 3))
#t
> (contains 1 '())
#f
> (contains 1 '(2 3))
#f
> (contains 1 '(2))
#f
> (contains 1 '(1))
#t
|#
(define (contains item lst)
  (cond [(null? lst) #f]
        [else (or (equal? (car lst) item)
                  (contains item (cdr lst))
               )
        ]
  )
)

#|
(search-table table item)
  table: a list of lists
  item: some value

  Returns a list containing the lists in 'table' which contain 'item'.
  The lists must appear in the same order they appear in 'table'.

> (search-table '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 1)
'((1 2 3) (5 3 2 1))
> (search-table '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 10)
'()
|#
; Feel free to change this signature to use the shorthand for defining functions
; (define (search-table ...) (...))
(define (search-table table item)
    (cond
        ;Base case: table is empty - return the empty list
        [(null? table) empty]
        
        ;if the table has one or more elements, use recursion
        [else
             ; This is the recursive call
             (let ([end (search-table (cdr table) item)])
                  (cond
                       ; If the first contains item, append end to it
                       [(contains item (car table)) (list* (car table) end)]

                       ; Otherwise, just return end
                       [else end]
                  )
              )
        ]
   )
)

#|
(item_at_pos? lst item pos)
    lst: a list of objects
    item: an object
    pos: an integer

    Returns a boolean indicating whether or not the object at index pos in lst
    is equal to item.

> (item_at_pos? '(1 2 3) 5 0)
#f
> (item_at_pos? '(1 2 3) 1 5)
#f
> (item_at_pos? '(1 2 3) 1 0)
#t
> (item_at_pos? '(1 2 3) 2 0)
#f
> (item_at_pos? '() 2 0)
#f
> (item_at_pos? '(2) 2 0.5)
#f
|#
(define (item_at_pos? lst item pos)
    (cond
        ; Auto return False if lst is empty
        [(null? lst) #f]
        
        ; Auto return False if index is not a non-negative integer
        [(not (and (integer? pos) (>= pos 0))) #f]
        
        ; Auto return False if pos is out of bounds
        [(> (+ pos 1) (length lst)) #f]

        ; Recurse on cdr with pos - 1 if pos > 0
        [(> pos 0)(item_at_pos? (cdr lst) item (- pos 1))]
        
        ; If pos = 0, return whether item == car lst
        [(= pos 0)(equal? (car lst) item)]
    )
)

#|
(search-table-2 table item [pos])
  table: a list of lists
  item: any value
  pos: (*optional* parameter) an index in a list. Default value is 0. 
  
  Returns a list containing the lists in 'table' which have 'item' 
  at position 'pos'.
  Lists with length <= 'pos' should, of course, not be included.
  The lists must appear in the same order they appear in 'table'.

> (search-table-2 '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 1 3)
'((5 3 2 1))
> (search-table-2 '((1 2 3) (4 2) (3 3 4) (5 3 2 1)) 1)
'((1 2 3))

Hint: look up "declaring optional arguments in Racket". The syntax is
pretty straight-forward. Note that optional arguments must appear
*after* all the required ones.
|#
(define (search-table-2 table item [pos 0])
    (cond
        ;Base case: table is empty - return the empty list
        [(null? table) empty]
        
        ;if the table has one or more elements, use recursion
        [else
             ; This is the recursive call
             (let ([end (search-table-2 (cdr table) item pos)])
                  (cond
                       ; If the first list has item at pos, append end to it
                       [(item_at_pos? (car table) item pos) (list* (car table) end)]

                       ; Otherwise, just return end
                       [else end]
                  )
              )
        ]
   )
)

#|
(max-sublist lst)
  lst: a list of numbers

  Returns the maximum sum of a sublist of numbers in 'lst'.
  A *sublist* of a list is a series of consecutive numbers in the list.

  Note that the *empty list* is a valid sublist of every list,
  and has a sum of 0.

  You may choose to use a brute-force O(n^3) algorithm for this question.
  However, for extra learning you're encouraged to try to find a O(n) algorithm.
  (Personally, I think the linear algorithm is actually easier to implement.)

> (max-sublist '(-1 10 -4 5 3 -100 6))
14  ; sum of '(10 -4 5 3)
> (max-sublist '(-4 -1 -2 -3))
0   ; sum of '()

Hint: you may find the "apply" function helpful.
|#
(define (max-sublist lst)
    (apply max (MS lst))
)

#|
(MS lst)
  lst: a list of numbers

  Helper function to determine the maximum possible sum of all the elements
  before an index (inclusive) for every index of a given
  list. MS(i) = Max[MS(i-1) + A[i] , A[i]]. Returns a list such that at every
  index i, the element at index i is MS(i) of lst.

|#
(define (MS lst)
    (cond
         ; Base case: lst is empty, return '(0)
         [(null? lst) '(0)]
         
         ; lst has one or more elements: recurse on the torso of the lst 
         [else
              (let([prior (MS (torso lst))])
                (append
                        prior
                        (list
                            (max
                                (+ (very-last prior) (very-last lst))
                                (very-last lst)
                             )
                        )
                 )
               )
         ]
     )
)

#|
(torso lst)
  lst: a list of objects

  Returns lst, excluding the last element. Returns the empty list when called
  on a singleton or the empty list.

> (torso '())
'()
> (torso '(1))
'()
> (torso '(-1 10 -4 5 3 -100 6))
'(-1 10 -4 5 3 -100)
> (torso '(-4 -1 -2 -3))
'(-4 -1 -2)
|#
(define (torso lst)
    (cond     
         ; Return the empty list if the list is empty
         [(null? lst) empty]

         ; Return the empty list if the list is a singleton (cdr is null)
         [(null? (cdr lst)) empty]

         ; If the list is two or more elements long, return everything but
         ;the very last element
         [else
              (let ([rev (reverse lst)])
                  (reverse (cdr rev))    
              )
         ]
    )
)

#|
(very-last lst)
  lst: a list of objects

  Returns the last element of lst. Throws an exception if called on the empty
  list.

> (very-last '(1))
1
> (very-last '(-1 10 -4 5 3 -100 6))
6
|#
(define (very-last lst)
        (list-ref lst (- (length lst) 1))
)