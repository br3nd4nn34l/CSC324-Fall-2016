#lang plai

; Some globals
(define x 20)
(define y "Hi")

; A function with a parameter shadowing the global definition
(define (f x) (+ x x))
; TODO: Uncomment and fill in the expected value
(test (f 2) 4)

; A function that returns a function, using its parameter
(define (maker n)
  (lambda (m) (- n m)))
(test ((maker 3) 10) -7)

; A function that returns a function, using a global variable
(define (maker-2 n)
  (lambda (m) (- x m)))
(test ((maker-2 3) 10) 10)

; A function that returns a function, using a local variable.
; Also, variable shadowing (why?)
(define (maker-3 n y)
  (let ([y "byeee"])
    (lambda (m) (+ n m (string-length y)))))
(test ((maker-3 3 15) 20) 28)

; A function that returns a higher-order function
(define (maker-4 n)
  (lambda (f x)
    (f (+ x n))))

; TODO: write your own test(s)!
(test ((maker-4 15) f 4) 38)
(test ((maker-4 17) f 1) 36)

; A function in a let, with shadowing
(define let-ex
  (let* ([x 15]
         [y (- x 6)])
    (lambda (y) (- x y))))
(test (let-ex 3) 12)  

; Combining everything
(define let-ex-2
  (let ([a 15]) ; a = 15
    (lambda (y)
      (let ([b (+ a 5)] ;b = 20
            [a 11]) ; a = 11
        (lambda (z) (+ (- z y) (- a b))))))); (z - y) + (11 - 20) = (z - y) - 9 = z - y - 9
(test ((let-ex-2 1) 0) -10);

; Last one - lexical vs. dynamic scope
; The const being used is the one defined outside the scope of the let
(define const 10)
(define (let-ex-3 y)
  (lambda (z) (+ y (- z const))))
(test
   (let* ([const 1]
         [y 2]
         [z 3])
     ((let-ex-3 1) 5))
   -4)
