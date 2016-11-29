{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer(..), Value(..), StateOp(..),
    runOp, (>>>), (>~>), returnVal
    )
    where 

import AList (AList, lookupA, insertA, updateA, removeA, hasKey)

-- A type representing the possible values stored in memory.
data Value = IntVal Integer |
             BoolVal Bool
             deriving Show

-- A type representing a container for stored "mutable" values.
type Memory = AList Integer Value

-- A type representing a pointer to a location in memory.
data Pointer a = P Integer deriving Show


-- Type class representing a type which can be stored in "Memory".
class Mutable a where
    -- Look up a value in memory referred to by a pointer.
    get :: Pointer a -> StateOp a
    lookupValue :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Pointer a -> a -> StateOp ()
    setValue :: Memory -> Pointer a -> a -> Memory 

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Integer -> a -> StateOp (Pointer a)
    -- DUNNO ABOUT THIS ONE BOYS
    defineValue :: Memory -> Integer -> a -> (Pointer a, Memory)


instance Mutable Integer where
    get (P location) = StateOp (\memory -> (lookupValue memory (P location), memory))

    lookupValue memory (P location) =
        if (not (hasKey memory location)) then
            error "Pointer not found in memory."
        else
            case lookupA memory location of
                IntVal x -> x

    set (P location) value = StateOp (\memory -> ((), setValue memory (P location) value))
 
    setValue memory (P location) value = 
        if (hasKey memory location) then
            updateA memory (location, IntVal value)
        else
            error "Pointer not found in memory."

    def location value = StateOp (\memory -> (defineValue memory location value))

    defineValue memory location value = 
        if (not (hasKey memory location)) then
            ((P location), insertA memory (location, IntVal value))
        else
            error "Attempting to overwrite an existing Pointer."


instance Mutable Bool where
    get (P location) = StateOp (\memory -> (lookupValue memory (P location), memory))

    lookupValue memory (P location) =
        if (not (hasKey memory location)) then
            error "Pointer not found in memory."
        else
            case lookupA memory location of
                BoolVal x -> x

    set (P location) value = StateOp (\memory -> ((), setValue memory (P location) value))

    setValue memory (P location) value = 
        if (hasKey memory location) then
            updateA memory (location, BoolVal value)
        else
            error "Pointer not found in memory."

    def location value = StateOp (\memory -> (defineValue memory location value))

    defineValue memory location value = 
        if (not (hasKey memory location)) then
            ((P location), insertA memory (location, BoolVal value))
        else
            error "Attempting to overwrite an existing Pointer."

data StateOp a = StateOp (Memory -> (a, Memory))

runOp :: StateOp a -> Memory -> (a, Memory)
runOp (StateOp op) mem = op mem

-- next
(>>>) :: StateOp a -> StateOp b -> StateOp b
op1 >>> op2 = StateOp (\s ->
    let (_, s1) = runOp op1 s
    in runOp op2 s1)

(>~>) :: StateOp a -> (a -> StateOp b) -> StateOp b
f >~> g = StateOp (\s ->
    let (x, s1) = runOp f s
        newStateOp = g x
    in runOp newStateOp s1)

returnVal :: a -> StateOp a
returnVal x = StateOp (\s -> (x, s))


alloc :: Mutable a => a -> StateOp (Pointer a)
alloc val = StateOp (\s ->
    runOp (def (generatePhreshNumber s 0) val) s)

generatePhreshNumber state acc =
    if (hasKey state acc) then
        generatePhreshNumber state (acc + 1)
    else
        acc

free :: Mutable a => Pointer a -> StateOp ()
free (P location) = StateOp (\s ->
    if (hasKey s location) then
        ((), removeA s location)
    else
        error "Pointer not found in memory.")

-- test
f :: Integer -> StateOp Bool
f x =
    def 1 4 >~> \p1 ->
    def 2 True >~> \p2 ->
    set p1 (x + 5) >>>
    get p1 >~> \y ->
    set p2 (y > 3) >>>
    get p2
