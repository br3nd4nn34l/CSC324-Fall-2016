{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Memory, Pointer(..), Value(..), Person,
    Mutable, get, set, def,
    StateOp(..), runOp,
    (>>>), (>~>), returnVal,
    alloc, free)
    where

import AList (AList, lookupA, insertA, updateA, containsA, freeA)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a person with two attributes:
-- age and whether they are a student or not.
data Person = Person Integer Bool deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer |
                 PersonPtr Integer Integer deriving Show

(@@) :: Pointer a -> (Pointer a -> Pointer b) -> Pointer b
(PersonPtr x y) @@ fxn = fxn (PersonPtr x y)

age :: Pointer Person -> Pointer Integer
age (PersonPtr x y) = (P x)

isStudent :: Pointer Person -> Pointer Bool 
isStudent (PersonPtr x y) = (P y)

-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    -- Result part of StateOp tuple must be of type a
    get :: Pointer a -> StateOp a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    -- Result part of StateOp tuple must be of type void
    set :: Pointer a -> a -> StateOp ()

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    -- Result part of StateOp tuple must be a Pointer of type a
    def :: Integer -> a -> StateOp (Pointer a)

extractInt :: Value -> Integer
extractInt (IntVal x) = x

extractBool :: Value -> Bool
extractBool (BoolVal b) = b

instance Mutable Integer where

    {-Should return the contents of the pointer => use lookup results in result position-}
    get (P p) = (StateOp lambda) where
        lambda = \mem -> ((if (containsA mem p) then
                               extractInt (lookupA mem p)
                           else
                               error "Doesn't exist"),
                           mem)

    {-This shouldn't return anything (i.e. void) => use () in result position-}
    set (P p) val = (StateOp lambda) where
        lambda = \mem -> ((),
                          (if (containsA mem p) then
                               updateA mem (p, (IntVal val))
                           else
                               error "Doesn't exist"))

    {-This should return the new pointer in the result position, 
    and the new memory in the memory position (throw an error if the input is used!)-}
    def int val = (StateOp lambda) where 
        lambda = \mem -> if (containsA mem int) then
                             error "Already exists"
                         else
                             (P int, insertA mem (int, (IntVal val)))

instance Mutable Bool where

    {-Should return the contents of the pointer => use lookup results in result position-}
    get (P p) = (StateOp lambda) where
        lambda = \mem -> ((if (containsA mem p) then
                               extractBool (lookupA mem p)
                           else
                               error "Doesn't exist"),
                           mem)

    {-This shouldn't return anything (i.e. void) => use () in result position-}
    set (P p) val = (StateOp lambda) where
        lambda = \mem -> ((),
                          (if (containsA mem p) then
                               updateA mem (p, (BoolVal val))
                           else
                               error "Doesn't exist"))

    {-This should return the new pointer in the result position, 
    and the new memory in the memory position (throw an error if the input is used!)-}
    def int val = (StateOp lambda) where 
        lambda = \mem -> if (containsA mem int) then
                             error "Already exists"
                         else
                             (P int, insertA mem (int, (BoolVal val)))

instance Mutable Person where
   {-Should return the contents of the pointer => use lookup results in result position-}
    get (PersonPtr x y) = (StateOp lambda) where
        lambda = \mem -> ((if (containsA mem x) && (containsA mem y) then
                               (Person (extractInt(lookupA mem x)) 
                                       (extractBool(lookupA mem y)))
                           else
                               error "Doesn't exist"),
                           mem)

    {-This shouldn't return anything (i.e. void) => use () in result position-}
    set (PersonPtr x y) (Person a i) = (StateOp lambda) where
        lambda = \mem -> if (containsA mem x) && (containsA mem y) then
                             let op1 = set (P x) a
                                 op2 = set (P y) i
                             in runOp (op1 >>> op2) mem
                         else
                             error "Doesn't exist."

    {-This should return the new pointer in the result position, 
    and the new memory in the memory position (throw an error if the input is used!)-}
    def int (Person a i) = (StateOp lambda) where
        lambda = \mem -> if (containsA mem int) then
                             error "Already exists"
                         else
                             let (P int1, mem1) = runOp (def int a) mem
                                 (P int2, mem2) = runOp (alloc i) mem1
                             in (PersonPtr int1 int2, mem2)

-- StateOp declarations and such
data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- StateOp chaining declarations
-- "return"
returnVal :: a -> StateOp a
returnVal a = (StateOp (\x -> (a, x)))

-- "then"
(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = (StateOp lambda) where
    lambda = \mem ->
        let mem1 = snd (runOp op1 mem)
        in  runOp op2 mem1

-- "bind"
(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
f >~> g = (StateOp lambda) where
    lambda = \mem ->
        let (result, mem1) = runOp f mem
            newOp = g result
        in (runOp newOp mem1)

-- | Memory allocation functions using StateOp
-- Allocating memory
alloc :: Mutable a => a -> StateOp (Pointer a)
alloc val = (StateOp lambda) where
    lambda = \mem -> let x = allocHelper mem [0..]
                     in runOp (def x val) mem

-- Finds the first integer from 0 and on which is not contained in the Memory
allocHelper :: Memory -> [Integer] -> Integer
allocHelper mem lst = if (containsA mem (head lst)) then
                          allocHelper mem (tail lst)
                      else
                          head lst

-- Deallocating memory
free :: Mutable a => Pointer a -> StateOp ()
free (P p) = (StateOp lambda) where
    lambda = \mem -> if (containsA mem p) then 
                         ((), (freeA mem p))
                     else
                         error "Doesn't exist"

-- Testing function defined in A3.pdf
personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x =
    -- not using alloc
    def 1 person >~> \personPointer ->
    get (personPointer @@ age) >~> \oldAge ->
    set (personPointer @@ age) x >>>
    get (personPointer @@ isStudent) >~> \stu ->
    get (personPointer @@ age) >~> \newAge ->
    set personPointer (Person (2 * newAge) (not stu)) >>>
    get personPointer >~> \newPerson ->
    get (personPointer @@ isStudent) >~> \newStu ->
    returnVal (oldAge, newStu, newPerson)

-- > fst (runOp (personTest (Person 2 True) 10) [])
-- (2, False, Person 20 False)
