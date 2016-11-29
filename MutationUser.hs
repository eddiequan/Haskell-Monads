{- Assignment 3 - Memory and Mutation

This file contains code which uses the mutation library found in Mutation.hs
-}

import Mutation (
    get, set, def, Mutable, Pointer, Memory,
    StateOp, returnVal, (>>>), (>~>), runOp
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

