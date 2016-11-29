{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}
module MutationUser (pointerTest) where

import Mutation (
    get, set, def, Mutable, Pointer(..), Memory,                                                        Value(..),
    StateOp(..), returnVal, (>>>), (>~>), runOp
    )

-- | Takes a number <n> and memory, and stores two new values in memory:
--   - the integer (n + 3) at location 100
--   - the boolean (n > 0) at location 500
--   Return the pointer to each stored value, and the new memory.
--   You may assume these locations are not already used by the memory.
pointerTest :: Integer -> StateOp (Pointer Integer, Pointer Bool)
pointerTest n = StateOp (\memory ->
    let (key, val) = runOp (def 100 (n + 3) >~> \x ->
                            def 500 (n > 0)) memory
    in ((P 100, P 500), val))

run :: StateOp a -> Memory -> (a, Memory)
run (StateOp f) mem = f mem

swap :: Mutable a => Pointer a -> Pointer a -> StateOp ()
swap (P p1) (P p2) = 
    StateOp (\s -> 
    let (val1, s1) = runOp (get (P p1)) s
        (val2, s2) = runOp (get (P p2)) s
    in
        --TODO: I'm getting a type error when I dont cast the type variable a as Integers. Why?
        runOp ((set ((P p1) :: Pointer Integer) val2) >~> \x ->
        set ((P p2) :: Pointer Integer) val1)
    s)

-- temporary test fixtures. remove later.
testMem :: Memory
testMem = [(1, IntVal 10), (2, IntVal 30), (3, IntVal 300), (4, BoolVal False)]
                                                                                                        
p1 :: Pointer Integer
p1 = P 1

p2 :: Pointer Integer
p2 = P 2 
        
p3 :: Pointer Integer
p3 = P 3
    
p4 :: Pointer Bool
p4 = P 4


