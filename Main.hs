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

main = runStateT (execute currentProgram) emptyUniverse
--main = compileToFile currentProgram "test.cpp"

