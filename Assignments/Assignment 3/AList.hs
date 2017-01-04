{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA,
    containsA,
    freeA
    )
    where


type AList a b = [(a, b)]

-- | Finds and removes the key from the given association list, returns the new
--   list.
--   Assumes the key is in the association list.
--   To be used for freeing memory in part 4.
freeA :: Eq a => AList a b -> a -> AList a b
freeA [] _       = []
freeA (a:as) key = if (fst a) == key then
                        as
                   else
                        a:(freeA as key)

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key | fst (head alist) == key = snd (head alist)
                  | otherwise               = lookupA (tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) | containsA alist key == False = alist ++ [(key, val)]
                         | otherwise                    = alist

-- | Returns a boolean value depending on whether or not the AList contain
--   the given key.
containsA :: Eq a => AList a b -> a -> Bool
containsA [] _      = False
containsA alist key | fst (head alist) == key = True
                    | otherwise               = containsA (tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA [] (key, val) = []
updateA (a:as) (key, val) = if (fst a) == key then 
                                (key, val) : as
                            else
                                a: (updateA as (key, val))
