module Interpreter where

import AbstractSyntax

import Data.Map
import Data.Char

type Variables = Map String StackEntry
type Stack = [StackEntry]
type Universe = (Stack, Variables, Bool)

emptyUniverse :: Bool -> Universe
emptyUniverse debug = ([], empty, debug)

printAsInt :: StackEntry -> Int
printAsInt (Integer i) = i
printAsInt (Char c) = ord c
printAsInt _ = error "printing only Integer or Char"

printAsCh :: StackEntry -> Char
printAsCh (Integer i) = chr i
printAsCh (Char c) = c
printAsCh _ = error "printing only Integer or Char"

getStack :: Universe -> Stack
getStack (stack,_,_) = stack

modStack :: (Stack -> Stack) -> Universe -> Universe
modStack f universe = (f (getStack universe), getVariables universe, getDebug universe)

getVariables :: Universe -> Variables
getVariables (_,var,_) = var

modVariables :: Universe -> (Variables -> Variables) -> Universe
modVariables universe f = (getStack universe, f (getVariables universe), getDebug universe)

getDebug :: Universe -> Bool
getDebug (_,_,debug) = debug

modDebug :: Universe -> (Bool -> Bool) -> Universe
modDebug universe f = (getStack universe, getVariables universe, f (getDebug universe))

getVariable :: String -> Universe -> StackEntry
getVariable name universe = (getVariables universe) ! name

setVariable :: String -> StackEntry -> Universe -> Universe
setVariable key value universe = modVariables universe (insert key value)

push :: StackEntry -> Universe -> Universe
push entry universe = (entry : getStack universe, getVariables universe, getDebug universe)

pop :: Universe -> Universe
pop = modStack tail

top :: Universe -> StackEntry
top universe = head $ getStack universe

debug :: String -> Universe -> Maybe String
debug str universe = if (getDebug universe)
                     then Just $ str <> "\t|stack=>" <> show (getStack universe)
                     else Nothing


