{-# LANGUAGE GADTs #-}
module Interpreter where

import ADT

import Control.Monad.State

push :: StackEntry -> State [StackEntry] ()
push e = do
    stack <- get
    put $ (e : stack)

pop :: State [StackEntry] StackEntry
pop = do
    stack <- get
    put $ tail stack
    return $ head stack

skip :: State [StackEntry] ()
skip = do
       stack <- get
       put stack

setVar :: StackEntry -> StackEntry -> State [StackEntry] ()
setVar = undefined

getVar :: StackEntry -> State [StackEntry] StackEntry
getVar = undefined

ap2 :: (StackEntry -> StackEntry -> StackEntry) ->  State [StackEntry] ()
ap2 f = do
      x <- pop
      y <- pop
      push (f x y)

ap1 :: (StackEntry -> StackEntry) ->  State [StackEntry] ()
ap1 f = do
      x <- pop
      push (f x)

runStackEntry :: StackEntry -> State [StackEntry] ()
runStackEntry (Function c) = execute c
runStackEntry _ = error "not executable"


execute :: Commands -> State [StackEntry] ()
execute [] = do
        stack <- get
        put stack
execute (x:xs) = do
        executeCommand x
        execute xs

executeCommand :: Command -> State [StackEntry] ()
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

executeCommand (Add) = ap2 (ap2StackEntry (Prelude.+))
executeCommand (Sub) = ap2 (ap2StackEntry (Prelude.-))
executeCommand (Mul) = ap2 (ap2StackEntry (Prelude.*))
executeCommand (Div) = ap2 (ap2StackEntry (Prelude.div))
executeCommand (Minus) = ap1 (ap1StackEntry (\x -> (-1) Prelude.* x))

executeCommand (Equal) = ap2 (apStackEntryEqual)
executeCommand (Larger) = ap2 (apStackEntryLarger)

executeCommand (And) = ap2 (ap2StackEntry andInt)
executeCommand (Or) = ap2 (ap2StackEntry orInt)
executeCommand (Not) = ap1 (ap1StackEntry (\x -> 1 - (max 1 x)))

executeCommand (If) = do
                 cond <- pop
                 func <- pop
                 if ifStackEntry cond
                 then
                   runStackEntry func
                 else
                   skip
executeCommand (While) = do
                 condf <- pop
                 func  <- pop

                 runStackEntry condf
                 cond  <- pop
                 if ifStackEntry cond
                 then do
                   runStackEntry func

                   push func
                   push condf
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
                      push y
                      push x
executeCommand (Rot) = do
                      x <- pop
                      y <- pop
                      z <- pop
                      push y
                      push x
                      push z
executeCommand (Pick) = do
                    idx <- pop
                    stack <- get
                    push $ stack !! stackEntry2int idx

executeCommand (PrintNum) = do
                            x <- pop
                            -- print should be here: putStrLn "foo"
                            skip
executeCommand (PrintStr str) = undefined
executeCommand (PrintCh) = undefined
executeCommand (ReadCh) = undefined
executeCommand (Flush) = skip

