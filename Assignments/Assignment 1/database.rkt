#| Assignment 1 - Racket Query Language  (due Oct 14, 11:50pm)
***Write the names, CDF accounts and student IDs for each of your group members below.***
Filip Tomin, tominfil, 1001329984
Brendan Neal, nealbre1, 1001160226
|#
#lang racket

; Namespace definition required for eval
(define ns (make-base-namespace))

; Function versions for common syntactic forms.
; *Use these in your queries instead of the syntactic forms!!!*
; (define (And x y) (and x y))
; (define (Or x y) (or x y))
; (define (If x y z) (if x y z))
(define-syntax Or
  (syntax-rules ()
    ((Or <args> ...)
     (or <args> ...))))

(define-syntax And
  (syntax-rules ()
    ((And <args> ...)
     (and <args> ...))))

(define-syntax If
  (syntax-rules ()
    ((If a b c)
     (if a b c))))

(define (tail lst) (rest lst))
(define (head lst) (first lst))
(define (no-attr-error) "Attribute does not exist!")

; Generic helper functions

#|
(number-of item lst)
   item: an object
   lst: a list of objects the same type as item
   Returns an integer representing the number of times item appears in lst.
> (number-of 1 '(1 2 3))
1
> (number-of 4 '(1 2 3))
0
>(number-of 1 '())
0
>(number-of 1 '(1 1 1))
3
|#
(define (number-of item lst)
  (count (λ(x) (equal? x item)) lst))

; TODO: After you have defined your macro(s), make sure you add each one
; to the provide statement.
(provide attributes
         tuples
         size
         SELECT
         FROM)

; Part 0: Semantic aliases

#|
(attributes table)
  table: a valid table (i.e., a list of lists as specified by assigment)
  Returns a list of the attributes in 'table', in the order they appear.
|#
(define (attributes table)
  ; First row of the table is the attributes, so just return first element
  (head table))

#|
(tuples table)
  table: a valid table
  Returns a list of all tuples in 'table', in the order they appear.
  Note: it is possible for 'table' to contain no tuples.
|#
(define (tuples table)
  ; Everything beyond the first index is a tuple
  ; Just return the tail of the list
  (tail table))

#|
(size table)
  table: a valid table
  Returns the number of tuples in 'table'.
|#
(define (size table)
  ; First element is the attribute specifier,
  ; so it cannot be included as part of size
  (- (length table) 1))


; Part I "WHERE" helpers; you may or may not wish to implement these.

#|
(get-value attribute-template target-attribute tup): 
  - attribute-template: a list of attributes
  - target-attribute: string (representing an attribute)
  - tup: a tuple
  Returns the value of the tuple corresponding to that attribute.
  If target-attribute is not in attribute-template, returns
  "Attribute does not exist!" (Piazza 181)
|#
(define (get-value attribute-template target-attribute tup)
  (let ([att-index (index-of target-attribute attribute-template)])
    (cond [(equal? att-index -1) (no-attr-error)]
          [else (list-ref tup att-index)])))

#|
(get-value attribute-template target-attributes tup): 
  - attribute-template: a list of attributes
  - target-attributes: a subset of attributes in attribute-template
  - tup: a tuple
  Returns a list of values of the tuple (in order of appearance
  in target-attributes) corresponding to the targeted attributes.
  Returns the empty list if there are no targeted attributes.
|#
(define (get-values attribute-template target-attributes tup)
  (cond [(null? target-attributes) empty]
        [else (cons (get-value
                     attribute-template
                     (head target-attributes)
                     tup)
                    (get-values
                     attribute-template
                     (tail target-attributes)
                     tup))]))

#|
(get-subtable target-attributes table): 
  - target-attributes: a subset of table's attributes
  - tup: a tuple
  Returns the tuples of table with only the attributes mentioned in target-attributes.
  If target-attributes is *, returns all the attributes to table.
|#
(define (get-subtable target-attributes table)
  (cond [(equal? * target-attributes) (get-subtable
                                       (attributes table)
                                       table)]
        [else (cons
               target-attributes
               (map (λ(tup) (get-values
                             (attributes table)
                             target-attributes
                             tup))
                    (tuples table)))]))

#|
(tups-satisfying f table)
  - f: a unary function that takes a tuple and returns a boolean value
  - table: a valid table
  Returns a new table containing only the tuples in 'table'
  that satisfy 'f'.
|#
(define (tups-satisfying f table)
  ; Want the head of the list to be the attributes of the table
  ; Want the tail of the list to be the satisying tuples
  (cons (attributes table)
        (filter f (tuples table))))

#|
(feed-tups func_list tup)
  - func_list: a list of procedures which take a tuple
  - tup: the tuple to pass into each procedure
  Returns a useable listed procedure from the output of replace
|#
(define (feed-tups func_list tup)
  (cond [(empty? func_list) func_list] ; If the function list is empty, return empty list.
        [(list? (head func_list)) ; If the first procedure is another list, the procedure is nested.
         (cons (feed-tups (head func_list) tup) (feed-tups (tail func_list) tup))]
        [else ; If not, feed the first element a tuple and recursively the rest of the elements, construct a list of tuple'd procedures.
         (cons ((head func_list) tup) (feed-tups (tail func_list) tup))]))

#|
(index-of x lst)
  - x: some object
  - lst: a list of objects
  Returns the index of x if x is in lst. If x is not in lst, return -1.
> (index-of 1 '(1 2 3))
0
> (index-of 2 '(1 2 3))
1
> (index-of 4 '(1 2 3))
-1
|#
(define (index-of x lst)
  (cond [(equal? #f (member x lst)) -1]
        [else (cond [(equal? (head lst) x) 0]
                    [else (+ 1 (index-of x (tail lst)))])]))

#|
A function 'replace-attr' that takes:
  - x 
  - a list of attributes
  and returns a function 'f' which takes a tuple and does the following:
    - If 'x' is in the list of attributes, return the corresponding value 
      in the tuple.
    - Otherwise, just ignore the tuple and return 'x'.
|#
(define (replace-attr x attr-lst)
  (cond [(not (member x attr-lst)) (λ(tup) x)]
        [else (λ(tup) (list-ref tup (index-of x attr-lst)))]))

; Cartesian Product Functions - Used for FROM clause (multiple tables)
#|
(cartesian-helper table1 table2)
  table1: list of lists [K1, K2, ..., Km]
  table2: list of lists [L1, L2, ..., Ln]
  Returns a list of the contatenation of all possible pairs of lists, in the 
  order [K1 + L1, K1 + L2, ..., K1 + Ln, K2 + L1, ..., K2 + Ln, ..., Km + Ln]
  If at least one of 'table1' and 'table2' is empty, their Cartesian product
  does not contain any lists.
> (cartesian-helper '((1 4) (2 10)) '((3 4 5) (2)))
'((1 4 3 4 5) (1 4 2) (2 10 3 4 5) (2 10 2))
|#
(define (cartesian-helper table1 table2)
  (cond
    [(null? table1) empty]
    [(null? table2) empty]
    [else
     (append-map (λ (lst1) (map (λ (lst2) (append lst1 lst2)) table2)) table1)]))

#|
(cartesian-product table1 table2)
  table1, table2: lists of lists in the table format specified by the assignment
  Returns a list of the following form:
      Head: concatenation of the attributes of table1 and table2, in that order
      Tail: cartesian product of the tuples of table1 and table2, in the form:
            (table1 data, table2 data)
> (cartesian-product '(("A" "B") (1 2) (3 4)) '(("C") (5)))
'(("A" "B" "C") (1 2 5) (3 4 5))
|#
(define (cartesian-product table1 table2) 
  (list* (append (attributes table1) (attributes table2))
         (cartesian-helper (tuples table1)
                           (tuples table2))))

; Use the multi-cartesian on the list of tables in FROM
#|
(multi-cartesian table-list)
  table-list: list of (lists of lists in the table format specified by the assignment)
  Returns a list of the following form:
      Head: concatenation of the attributes of all the tables in table-list,
            in order of appearance, renaming attributes that are shared between tables.
      Tail: cartesian product of the tuples of all the tables in table-list.
> (multi-cartesian (list '(("A" "B") (1 2) (3 4)) '(("C") (5)) '(("D") (6))))
'(("A" "B" "C" "D") (1 2 5 6) (3 4 5 6))
|#
(define (multi-cartesian table-list)
  (cond [(equal? 1 (length table-list))(head table-list)]
        [else (cartesian-product
               (head table-list)
               (multi-cartesian (tail table-list)))]))

#|
(same-attribute-names tables)
     tables: list of tables in the format specified by the assignment
     Returns a list of attributes that appear in more than one table in tables.
> (same-attribute-names (list '(("A" "C") (1 2)) '(("A" "B" "C") (4 5 6))))
'("A" "C")
> (same-attribute-names (list '(("A" "C") (1 2)) '(("A" "B" "C") (4 5 6)) '(("B" "D") (3 4))))
'("A" "C" "B")
|#
(define (same-attribute-names tables)
  (let ([attribute-list (map attributes tables)])
    (let ([merged-attributes (apply append attribute-list)])
      (remove-duplicates (filter
                          (λ(attr) (> (number-of attr merged-attributes) 1))
                          merged-attributes)))))

#|
(rename-attribute table original-name new-name)
     table: a table in the format specified by the assignment
     original-name: the attribute to be renamed
     new-name: the new name of the attribute to be renamed
     Returns a new table with attribute with name original-name renamed to new-name.
> (rename-attribute '(("A" "C") (1 2)) "A" "Apples")
'(("Apples" "C") (1 2))
|#
(define (rename-attribute table original-name new-name)
  (let ([attrs (attributes table)])
    (let ([renamed-attrs (map (λ(attr) (cond [(equal? attr original-name) new-name]
                                             [else attr]))
                              attrs)])
      (cons renamed-attrs
            (tuples table)))))

#|
(rename-attributes table original-names new-names)
     table: a table in the format specified by the assignment
     original-name: the attributes to be renamed
     new-name: the new name of the attributes to be renamed (in the same order as original-name)
     Returns a new table with attribute with original-names renamed to new-names.
> (rename-attributes '(("A" "C") (1 2)) '("A" "C") '("Apples" "Cranberries"))
'(("Apples" "Cranberries") (1 2))
|#
(define (rename-attributes table original-names new-names)
  (cond [(not (equal? (length original-names) (length new-names)))
         "new-names must be the same length as original-names"]
        [(and (empty? original-names) (empty? new-names)) table]
        [else (let ([first-stage (rename-attribute
                                  table
                                  (head original-names)
                                  (head new-names))])
                (rename-attributes
                 first-stage
                 (tail original-names)
                 (tail new-names)))]))

; Starter for Part 3; feel free to ignore!

; Returns list of functions
(define-syntax replace
  (syntax-rules ()
    ; The recursive step, when given a compound expression
    [(replace (expr ...) table)
     (list (replace expr table) ... )]
    
    ; The base case, when given just an atom
    [(replace atom table)
     (replace-attr atom (attributes table))]))

; Start of SQL-like syntax macros
(define-syntax FROM
  (syntax-rules ()
    ; Just return the table if it is a singleton
    [(FROM table) (multi-cartesian (list table))]
    
    ; The case where each table is given a name (multi-product)
    [(FROM [table table-name] ...)
     ; Find the common attributes between all the tables
     (let ([common-attributes (same-attribute-names (list table ...))])
       ; Use the rename attributes method to ONLY rename attributes that are common
       ; between the two tables (using dot notation)
       (let ([tables-to-cross (list
                               (rename-attributes
                                table
                                common-attributes
                                (map (λ(x) (string-append table-name "." x))
                                     common-attributes)) ... )])
         (multi-cartesian tables-to-cross)))]))

(define-syntax SELECT
  (syntax-rules (FROM WHERE ORDER BY)
    
    ; SELECT FROM WHERE ORDER BY (Filtered then Sorted Basic Query)
    [(SELECT <attr-lst> FROM <tables> ... WHERE <cond> ORDER BY <expr>)
     (let ([table-to-sort (SELECT * FROM <tables> ... WHERE <cond>)])
       (SELECT <attr-lst> FROM table-to-sort ORDER BY <expr>))]
    
    ; SELECT FROM WHERE (Filtered Basic Query)
    [(SELECT <attr-lst> FROM <tables> ... WHERE <cond>)
     (let ([table-to-filter (SELECT * FROM <tables> ...)])
       (let ([pred (replace <cond> table-to-filter)])
         (cond [(not (list? pred))
                (SELECT <attr-lst> FROM (tups-satisfying pred table-to-filter))]
               [else
                (SELECT <attr-lst> FROM (tups-satisfying
                                         (λ(tuple) (eval (feed-tups (replace <cond> table-to-filter) tuple) ns))
                                         table-to-filter))])))]
    
    ; SELECT FROM ORDER BY (Sorted Basic Query)
    [(SELECT <attr-lst> FROM <tables> ... ORDER BY <expr>)
     (let ([table-to-sort (SELECT * FROM <tables> ...)])
       (let ([fxn-lst (replace <expr> table-to-sort)])
         (cond [(not (list? fxn-lst)) ; Just a singular attribute
                (SELECT <attr-lst> FROM (cons (attributes table-to-sort)
                                              (sort
                                               (tuples table-to-sort)
                                               > ;order of sorting is decreasing
                                               #:key (λ(tuple) (fxn-lst tuple)))))]
               [else ; A compound expression
                (let ([key-retriever (λ(tuple) (eval (feed-tups fxn-lst tuple) ns))]) ; Build the function to sort on
                  (SELECT <attr-lst> FROM (cons (attributes table-to-sort)
                                                (sort
                                                 (tuples table-to-sort)
                                                 > ;order of sorting is decreasing
                                                 #:key (λ(tup) (key-retriever tup))))))])))]
    
    ; SELECT FROM (Basic Query)
    [(SELECT <attr-lst> FROM <tables> ...)
     (let ([resulting-table (FROM <tables> ...)])
       (get-subtable
        <attr-lst>
        resulting-table))]))
