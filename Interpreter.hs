{-# LANGUAGE GADTs #-}
module Interpreter where

import AbstractSyntax

import Control.Monad.State
import System.IO
import Data.Map
import Data.Char


type Variables = Map String StackEntry
type Stack = [StackEntry]
type Universe = (Stack, Variables, Bool)
type MyState a = StateT Universe IO a

printAsInt :: StackEntry -> Int
printAsInt (Integer i) = i
printAsInt (Char c) = ord c
printAsInt _ = error "printing only Integer or Char"

printAsCh :: StackEntry -> Char
printAsCh (Integer i) = chr i
printAsCh (Char c) = c
printAsCh _ = error "printing only Integer or Char"

emptyUniverse :: Bool -> Universe
emptyUniverse debug = ([], empty, debug)

getStack :: MyState Stack
getStack = do
         (stack,_,_) <- get
         return $ stack

getVariables :: MyState Variables
getVariables = do
         (_,var,_) <- get
         return $ var

getDebug :: MyState Bool
getDebug = do
         (_,_,debug) <- get
         return $ debug

getVariable :: String -> Variables -> StackEntry
getVariable ch var = var ! ch

setVariable :: String -> StackEntry -> Variables -> Variables
setVariable = insert

getStateVariable :: String -> MyState StackEntry
getStateVariable ch = do
             var <- getVariables
             return $ getVariable ch var

setStateVariable :: String -> StackEntry -> MyState ()
setStateVariable key value = do
             var <- getVariables
             stack <- getStack
             debug <- getDebug
             put (stack, setVariable key value var, debug)

push :: StackEntry -> MyState ()
push e = do
    (stack,var,debug)<- get
    put $ ((e : stack), var, debug)

pop :: MyState StackEntry
pop = do
    (stack,var,debug)<- get
    put $ (tail (stack), var, debug)
    return $ head stack

skip :: MyState ()
skip = do
       stack <- get
       put stack

setVar :: StackEntry -> StackEntry -> MyState ()
setVar (Varadr ch) = setStateVariable [ch]
setVar (Char ch) = setStateVariable [ch]
setVar _ = error "setVar not supported operation"

getVar :: StackEntry -> MyState StackEntry
getVar (Varadr ch) = getStateVariable [ch]
getVar (Char ch) = getStateVariable [ch]
getVar _ = error "getVar not supported operation"

ap2 :: (StackEntry -> StackEntry -> StackEntry) ->  MyState ()
ap2 f = do
      y <- pop
      x <- pop
      push (f x y)

ap1 :: (StackEntry -> StackEntry) ->  MyState ()
ap1 f = do
      x <- pop
      push (f x)

runStackEntry :: StackEntry -> MyState ()
runStackEntry (Function c) = execute c
runStackEntry _ = error "not executable"


execute :: Commands -> MyState ()
execute [] = do
        stack <- get
        put stack
execute (x:xs) = do
        stack <- getStack
        debug <- getDebug
        if debug then liftIO $ putStrLn $ show x <> "\t|stackB=>" <> show stack
        else skip
        executeCommand x
        stack2 <- getStack
        if debug then liftIO $ putStrLn $ show x <> "\t|stackA=>" <> show stack2
        else skip
        execute xs

executeCommand :: Command -> MyState ()
executeCommand (PushFunction c) = push (Function c)
executeCommand (PushVaradr c) = push (Varadr c)
executeCommand (PushInteger x) = push (Integer x)
executeCommand (PushChar x) = push (Char x)

executeCommand (AssignVar) = do
               var <- pop
               value <- pop
               setVar var value
executeCommand (PushVar) = do
                  var <- pop
                  value <- getVar var
                  push value
executeCommand (RunFunction) = do
                  f <- pop
                  runStackEntry f

executeCommand (Add) = ap2 (ap2StackEntry (+))
executeCommand (Sub) = ap2 (ap2StackEntry (-))
executeCommand (Mul) = ap2 (ap2StackEntry (*))
executeCommand (Div) = ap2 (ap2StackEntry (div))
executeCommand (Minus) = ap1 (ap1StackEntry (\x -> (-1) * x))

executeCommand (Equal) = ap2 (apStackEntryEqual)
executeCommand (Larger) = ap2 (apStackEntryLarger)

executeCommand (And) = ap2 (ap2StackEntry andInt)
executeCommand (Or) = ap2 (ap2StackEntry orInt)
executeCommand (Not) = ap1 (ap1StackEntry (\x -> if x == 0 then -1 else 0))

executeCommand (If) = do
                 func <- pop
                 cond <- pop
                 if ifStackEntry cond
                 then
                   runStackEntry func
                 else
                   skip
executeCommand (While) = do
                 func  <- pop
                 condf <- pop

                 runStackEntry condf
                 cond  <- pop
                 if ifStackEntry cond
                 then do
                   runStackEntry func

                   push condf
                   push func
                   executeCommand While
                 else
                   skip

executeCommand (Dup) = do
                       x <- pop
                       push x
                       push x
executeCommand (Del) = do
                      _ <- pop
                      skip
executeCommand (Swap) = do
                      x <- pop
                      y <- pop
                      push x
                      push y
executeCommand (Rot) = do
                      x <- pop
                      y <- pop
                      z <- pop
                      push y
                      push x
                      push z
executeCommand (Pick) = do
                    idx <- pop
                    stack <- getStack
                    push $ stack !! (stackEntry2int idx)

executeCommand (PrintNum) = do
                     n <- pop
                     liftIO $ putStr $ show $ printAsInt n
executeCommand (PrintStr str) = liftIO $ putStr str
executeCommand (PrintCh) = do
                     c <- pop
                     liftIO $ putStr $ [printAsCh c]
executeCommand (ReadCh) = do
                     input <- liftIO $ getChar
                     push (Integer $ ord input)
executeCommand (Flush) = liftIO $ hFlush stdout

