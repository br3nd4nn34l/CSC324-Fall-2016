#| Lab 4: Higher-order list-ref

Complete each of the following functions.
You're encourage to write your own tests for these ones.
|#
#lang plai

#|
(list-ref-one i)
  i: a non-negative integer

  Returns a function which takes a list and returns
  the item in the list at index 'i', or "ERROR" if
  'i' is out of bounds.
> ((list-ref-one 1) '("a" "b" "c" "d" "e"))
"b"
|#


#|
(list-ref-many is)
  is: a list of non-negative integers

  Returns a function which takes a list and returns
  the items in the list at the indexes specified by 'is',
  in the order that 'is' is given; or, it returns "ERROR"
  if any one of the items in 'is' is out of bounds.
> ((list-ref-many '(1 2 4 0 1)) '("a" "b" "c" "d" "e"))
'("b" "c" "e" "a" "b")
|#


#|
(list-ref-rec is)
  is: a possibly nested list of non-negative integers.

  Same as list-ref-many, except now 'is' can contain
  nested lists instead of just integers.

  It may be easier if you define this function to also
  work for single integers in addition to nested lists.

> ((list-ref-rec '(1 2 (3 (4 0)) 1)) '("a" "b" "c" "d" "e"))
'("b" "c" ("d" ("e" "a")) "b")
|#


#|
(list-ref-data data)
  data: a possibly nested list containing arbitrary data.

  Same as list-ref-rec, except any time an atom which is
  not an index or in range is encountered, return that atom
  rather than returning (void).

> ((list-ref-data '(1 "heeey" (3 (4 -10.5)) 7)) '("a" "b" "c" "d" "e"))
'("b" "heeey" ("d" ("e" -10.5)) 7)
|#
