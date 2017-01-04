; Note the use of "plai" for testing purposes
#lang plai


#|
We say that m is a factor of n if the remainder when n is divided by m is 0.
A prime number is a natural number > 1 whose only factors are 1 and itself.

Implement the functions below to create a prime-listing function.

Do not use explicit recursion anywhere in your code; instead,
use higher-order functions as much as possible.
|#


#|
(factor? m n)
  m, n: positive natural numbers

  Returns #t if and only if m is a factor of n.

HINT: how do you calculate remainders in Racket?
|#
(define (factor? m n)
  (equal? 0 (modulo n m)))

; Tests
(test (factor? 2 6) #t)
(test (factor? 1 6) #t)
(test (factor? 6 2) #f)
(test (factor? 3 10) #f)

#|
(factors n)
  n: positive natural number

  Returns a list of all factors of n, in increasing order.

HINT 1: use (range n) to return a list of numbers from 0 to n-1.
HINT 2: use a lambda here to create a new unary predicate that
returns true if and only if its argument is a factor of n.
|#
(define (factors n)
  (let ([ints-to-check (map (λ(x) (+ x 1))
                            (range (sqrt n)))])
    (let ([first-half-of-factors (filter (λ(x) (factor? x n))
                                         ints-to-check)])
      (let ([second-half-of-factors (map (λ(x) (/ n x))
                                         first-half-of-factors)])
        (remove-duplicates (append first-half-of-factors (reverse second-half-of-factors)))))))
      

; Tests
(test (factors 5) '(1 5))
(test (factors 12) '(1 2 3 4 6 12))
(test (factors 1) '(1))

#|
(prime? n)
  n: positive natural number

  Returns #t if and only if n is prime.
|#
(define (prime? n)
  (equal? (factors n)
          (list 1 n)))

; Tests
(test (prime? 2) #t)
(test (prime? 10) #f)

#|
(primes-up-to n)
  n: positive natural number

  Returns a list of all prime numbers up to and including n.
|#

(define (primes-up-to n)
  (let ([ints-to-filter (map (λ(x) (+ x 1))
                             (range n))])
    (filter (λ(x) (prime? x))
            ints-to-filter)))

; Tests
(test (primes-up-to 20) '(2 3 5 7 11 13 17 19))

#| BONUS!
The above implementation we've guided you through is quite inefficient:
at every number, you calculate *all* of its factors.

Improve your algorithm for primes-up-to. :)

Idea: keep a list of prime numbers found thus far, only need to evaluate
each new number against all of these primes to check if it is not prime|#
(define (improved-primes-up-to n)
  (let ([primes ])
    ))