{- Assignment 3 - Memory and Mutation

This file contains the code responsible for working with association lists,
which you will use as the data structure for storing "mutable" data.
-}

-- **YOU MUST ADD ALL FUNCTIONS AND TYPES TO THIS LIST AS YOU CREATE THEM!!**
module Mutation (
    Mutable, get, set, def,
    Memory, Pointer
    )
    where

import AList (AList, lookupA, insertA, updateA, hasKey)

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
    get :: Memory -> Pointer a -> a

    -- Change a value in memory referred to by a pointer.
    -- Return the new memory after the update.
    set :: Memory -> Pointer a -> a -> Memory

    -- Create a new memory location storing a value, returning a new pointer
    -- and the new memory with the new value.
    -- Raise an error if the input Integer is already storing a value.
    def :: Memory -> Integer -> a -> (Pointer a, Memory)


instance Mutable Integer where
    get memory (P location) =
        if (not (hasKey memory location)) then
            error "Pointer not found in memory."
        else
            case lookupA memory location of
                IntVal x -> x

    set memory (P location) value = 
        if (hasKey memory location) then
            updateA memory (location, IntVal value)
        else
            error "Pointer not found in memory."

    def memory location value = 
        if (not (hasKey memory location)) then
            ((P location), insertA memory (location, IntVal value))
        else
            error "Attempting to overwrite an existing Pointer."


instance Mutable Bool where
    get memory (P location) =
        if (not (hasKey memory location)) then
            error "Pointer not found in memory."
        else
            case lookupA memory location of
                BoolVal x -> x

    set memory (P location) value = 
        if (hasKey memory location) then
            updateA memory (location, BoolVal value)
        else
            error "Pointer not found in memory."

    def memory location value = 
        if (not (hasKey memory location)) then
            ((P location), insertA memory (location, BoolVal value))
        else
            error "Attempting to overwrite an existing Pointer."

