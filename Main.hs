module Main where

import qualified System.Environment

import AbstractSyntax
import InterpreterS
import Compiler
import Examples

currentProgram :: Commands
currentProgram = tafels

debug_arg :: [Prelude.String] -> Prelude.Bool
debug_arg = Prelude.elem "debug"

main = do
  args <- System.Environment.getArgs
  if (Prelude.elem "interpreter" args)
  then do 
       run currentProgram (debug_arg args)
       Prelude.pure ()
  else compileToFile currentProgram (debug_arg args) "test.cpp"

