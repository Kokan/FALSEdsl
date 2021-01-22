module Main where

import qualified Prelude
import qualified Control.Monad.State
import qualified System.Environment

import AbstractSyntax
import Interpreter
import Compiler
import Examples

currentProgram :: Commands
currentProgram = program1

debug_arg :: [Prelude.String] -> Prelude.Bool
debug_arg = Prelude.elem "debug"

main = do
  args <- System.Environment.getArgs
  if (Prelude.elem "interpreter" args)
  then do 
       Control.Monad.State.runStateT (execute currentProgram) (emptyUniverse (debug_arg args))
       Prelude.putStrLn ""
  else compileToFile currentProgram (debug_arg args) "test.cpp"

