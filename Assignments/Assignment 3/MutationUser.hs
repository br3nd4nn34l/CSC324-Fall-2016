{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
module MutationUser (
    pointerTest,
    swap,
    swapCycle
    )
    where

import Mutation (
    get, set, def, Mutable, Pointer(..), StateOp(..), Memory, (>>>), (>~>),
    returnVal, Value(..), runOp, alloc, free)

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
--   Original:
-- pointerTest :: Integer -> Memory -> ((Pointer Integer, Pointer Bool), Memory)
pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
pointerTest num = def 100 (num + 3) >~> \p1 ->
                      def 500 (num > 0) >~> \p2 ->
                      returnVal (p1, p2)

-- | Given two pointers, swaps the values that the pointers point to in memory
swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap p1 p2 = (StateOp lambda) where
    lambda = \mem ->
        let (x, mem1) = runOp (get p1) mem
            (y, mem2) = runOp (get p2) mem1
            (_, mem3) = runOp (set p2 x) mem2
            (_, mem4) = runOp (set p1 y) mem3
        in ((), mem4)

-- | Given a list of pointers, left shift the values that they point to.
swapCycle :: Mutable a => [Pointer a] -> StateOp ()
swapCycle (p:[]) = (StateOp lambda) where
    lambda = \mem ->
        let (_, mem1) = runOp (get p) mem
        in ((), mem1)
swapCycle (p1:p2:ps) = (StateOp lambda) where
    lambda = \mem ->
        let (_, mem1) = runOp (swap p1 p2) mem
        in runOp (swapCycle (p2:ps)) mem1
