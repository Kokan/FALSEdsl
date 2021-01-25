module InterpreterS where

import AbstractSyntax
import qualified Interpreter as Interp

import Control.Monad.State
import System.IO
import Data.Char


type Variables = Interp.Variables
type Stack = Interp.Stack
type Universe = Interp.Universe
type MyState a = StateT Universe IO a

getStack :: MyState Stack
getStack = gets Interp.getStack

getVariables :: MyState Variables
getVariables = gets Interp.getVariables

getDebug :: MyState Bool
getDebug = gets Interp.getDebug

getStateVariable :: String -> MyState StackEntry
getStateVariable ch = gets (Interp.getVariable ch)

setStateVariable :: String -> StackEntry -> MyState ()
setStateVariable key value = modify (Interp.setVariable key value)

push :: StackEntry -> MyState ()
push e = modify (Interp.push e)

pop :: MyState StackEntry
pop = do
    top <- gets (Interp.top)
    modify (Interp.pop)
    pure top

skip :: MyState ()
skip = return ()

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
runStackEntry x = error $ show x <> " not executable"

run :: Commands -> Bool -> IO ()
run cmd is_debug = do
                   runStateT (execute cmd) (Interp.emptyUniverse is_debug)
                   pure ()

debug_print :: String -> MyState ()
debug_print str = do 
          msg <- gets (Interp.debug str)
          case msg of
             Just m -> liftIO $ putStrLn $ m
             Nothing -> skip


execute :: Commands -> MyState ()
execute [] = skip
execute (x:xs) = do
        debug_print (show x)
        executeCommand x
        debug_print (show x)
        execute xs

executeCommand :: Command -> MyState ()
executeCommand (Push s) = push s
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
               liftIO $ putStr $ show $ Interp.printAsInt n
executeCommand (PrintStr str) = liftIO $ putStr str
executeCommand (PrintCh) = do
               c <- pop
               liftIO $ putStr $ [Interp.printAsCh c]
executeCommand (ReadCh) = do
               input <- liftIO $ getChar
               push (Integer $ ord input)
executeCommand (Flush) = liftIO $ hFlush stdout

