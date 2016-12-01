module CompoundMutationUser (personTest) where 

import CompoundMutation (
  Mutable, get, set, def, 
  Memory, Pointer(..), Value(..), StateOp(..),
  runOp, (>>>), (>~>), returnVal, Person(..), (@@), age, isStudent, 
  alloc, free)

personTest :: Person -> Integer -> StateOp (Integer, Bool, Person)
personTest person x = 
  -- not using alloc, but we could
  def 1 person >~> \personPointer ->
  get (personPointer @@ age) >~> \oldAge ->
  set (personPointer @@ age) x >>>
  get (personPointer @@ isStudent) >~> \stu ->
  get (personPointer @@ age) >~> \newAge ->
  set personPointer (Person (2 * newAge) (not stu)) >>>
  get personPointer >~> \newPerson ->
  get (personPointer @@ isStudent) >~> \newStu ->
  returnVal (oldAge, newStu, newPerson)
