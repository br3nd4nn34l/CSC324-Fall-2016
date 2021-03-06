{- Exercise 5, due Nov 5, 11:50pm

General exercise instructions:
- Exercises must be done *individually*.
- You may not import any Haskell libraries, unless explicitly told to.
- You may write helper functions freely; in fact, you are encouraged
  to do so to keep your code easy to understand.
- Your grade will be determined by our automated testing.
  You can find some sample tests on the course webpage.
- Submit early and often! MarkUs is rather slow when many people
  submit at once. It is your responsibility to make sure your work is
  submitted on time.
- No late submissions will be accepted!

In this exercise, you'll play around more with more advanced Haskell types.
-}

module Ex5 (Tree(Empty, Node), treeSum, eitherMap) where

-- A modified tree type where every node may or may not
-- contain an integer.
data Tree = Empty | Node (Maybe Integer) Tree Tree deriving Show


{-Helper function to let treeSum add Maybe x values together.-}
ignoreNothings :: Maybe Integer -> Integer
ignoreNothings (Nothing) = 0
ignoreNothings (Just x) = x

-- The Maybe type is already imported from the standard library,
-- but here it is as a reference. Leave this commented out.
-- Maybe a = Nothing | Just a

-- Question 1
-- Implement 'treeSum', which takes a tree and computes the
-- sum of the values in the tree, ignoring all "Nothing" values.
-- Two special cases:
-- 1) If the tree is empty, return Just 0
-- 2) If the tree is non-empty but only has "Nothing" values,
--    return Nothing
treeSum :: Tree -> Maybe Integer
treeSum (Empty) = Just 0
treeSum (Node Nothing Empty Empty) = Nothing
treeSum (Node (Just x) left right) = Just ((ignoreNothings (Just x)) + (ignoreNothings (treeSum left)) + (ignoreNothings (treeSum right)))


-- Question 2
-- This question explores another classic type in Haskell: Either.
-- First, read through the few paragraphs on Either in LYAH:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- (about halfway down the page, do a search for "Either a b").
-- For your reference - leave commented out.
-- data Either a b = Left a | Right b

-- Now, implement a function 'eitherMap', which takes an Either value
-- and two functions, and chooses one of the functions to apply,
-- depending on which is compatible with the inner type.
-- The type signature reveals more, as does the sample test.
eitherMap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
eitherMap atoc btod (Left a) = Left (atoc a)
eitherMap atoc btod (Right b) = Right (btod b)
