#lang racket

; CONSTANTS FOR STRINGS FOR EASY MODIFICATION
(define attribute-string "attribute")
(define line-string "line")
(define badtag-warning "invalid tag")
(define badmsg-warning "invalid message")

; QUESTION 3 (fake constructor)
#|
WE NEED ONE-TWO PARAGRAPHS OF DOCSTRING HERE

We will be using a tagging system to differentiate between
"lines" that represent an attribute and those that do not.
This will allow us to tolerate any number of 

The tagging system will work as follows:
    var-name value tag - means that var-name is assigned to value and tagged with tag

Tags will have the following values (specified at the top of this file):
    attribute-string - designates the associated "line" as an attribute
    line-string - designates the associated "line" as *not* an attribute

All the "lines" entered into the pseudo-constructor will be considered,
and those that are tagged with "attribute" will be identified as class attributes.
All other intermediate "lines" that are not attributes must be tagged as
"line". When the constructor is called, it generates a function that takes a
message and returns the appropriate value. The message must be the string
form of the attribute, otherwise badmsg-warning will be returned.

Examples:

> (class-construct Cow (init (cowname cowcolor cowprice)
                             (name cowname "attribute")
                             (color cowcolor "attribute")
                             (price cowprice "attribute")))
> (define myCow (Cow "Cow" "White" 2))
> (myCow "name")
"Cow"
> (myCow "color")
"White"
> (myCow "price")
2
> (myCow "apples")
"invalid message"
|#
(define-syntax class-construct
  (syntax-rules (init)
    [(class-construct <Class> (init (<attr> ...)
                                    (<var> <body> <tag>)
                                    ...))
     (define (<Class> <attr> ...)
       (let* ([<var> <body>]
              ...
              [lookup-table (list (list (id->string <var>)
                                        <tag>)
                                  ...)]
              [attributes (get-attributes lookup-table)])
         (λ(msg)(cond [(member msg attributes)
                       (cond [(equal? (id->string <var>) msg)
                              <body>]
                             ...)]
                      [else badmsg-warning]))))]))


; HELPER FUNCTIONS BELOW THIS LINE ===========================================
#|
Returns a list of variables (by name) that were tagged with attribute-string.
If any tag is invalid, it will return badtag-warning.
|#
(define (get-attributes lookup-table)
  (let ([is-attribute-lst (map (λ(entry)(is-attribute entry))
                               lookup-table)])
    (if (member badtag-warning is-attribute-lst)
        badtag-warning
        (map (λ(entry)(first entry))
             (filter (λ(entry)(is-attribute entry))
                     lookup-table)))))

#|
Returns true if lookup-entry (in the style of '(variable-name tag))
has the tag attribute-string. If the tag is invalid, returns badtag-warning.
|#
(define (is-attribute lookup-entry)
  (cond [(equal? attribute-string (last lookup-entry)) #t]
        [(equal? line-string (last lookup-entry)) #f]
        [else badtag-warning]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))

; HELPER FUNCTIONS END HERE ====================================================

; EXAMPLE USAGE TO TRANSLATE PYTHON EXAMPLE IN ASSIGNMENT HANDOUT

; Using the above macro to translate the class definition given in the assignment handout
; Defining MyClass and its constructor
(class-construct MyClass (init (a b)
                             (r (f a) "line")
                             (x r "attribute")
                             (y (list b 100 r) "attribute")
                             (z "You are cool" "attribute")))
; Defining f
(define (f r) (+ r 5))

; Defining an example instance of MyClass
(define myClassExample (MyClass 1 2))

; Getting x, y, and z from the example instance
(myClassExample "x")
(myClassExample "y")
(myClassExample "z")