module Main where

import Prelude
import qualified Data.Stack
import Control.Monad.State

import ADT
import Interpreter

program :: Commands
program = [PushInteger 1, PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushFunction [Dup, PushInteger 10, Larger], While]

program1 :: Commands
program1 = [PrintStr "start=", Flush, ReadCh, PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushFunction [Dup, PushInteger 10, Larger], While]


--main :: Prelude.IO ()
main = runStateT (execute program1) emptyUniverse

