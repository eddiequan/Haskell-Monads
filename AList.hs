{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}
module AList (
    AList,
    lookupA,
    insertA,
    updateA
    )
    where


type AList a b = [(a, b)]

-- | Returns the value in the association list corresponding to the given key.
--   Assumes that the key is in the association list.
lookupA :: Eq a => AList a b -> a -> b
lookupA alist key = 
    let (a, b) = head alist
    in
    if (a == key) then b else lookupA (tail alist) key

-- | Returns a new association list which is the old one, except with 
--   the new key-value pair inserted. However, it returns the *same* list
--   if the key already exists in the list.
insertA :: Eq a => AList a b -> (a, b) -> AList a b
insertA alist (key, val) =
    if (hasKey alist key) then
        alist
    else
        (key, val) : alist

-- | Returns a new association list which is the old one, except with 
--   the value corresponding to the given key changed to the given new value.
--   However, it returns the *same* list if the key doesn't appear in the list.
updateA :: Eq a => AList a b -> (a, b) -> AList a b
updateA alist (key, val) =
    map (\association -> 
            let (a, b) = association in
            if (a == key) then (a, val) else (a, b)) alist


-- Helpers
hasKey :: Eq a => AList a b -> a -> Bool
hasKey [] key = False
hasKey alist key =
    let (a, b) = head alist
    in
    if (a == key) then True else hasKey (tail alist) key
