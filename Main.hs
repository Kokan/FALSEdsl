module Main where

import Prelude
import qualified Data.Stack
import Control.Monad.State

import ADT
import Interpreter

program :: Commands
program = [PushInteger 1, PushFunction [Dup, PushInteger 1, Add], PushFunction [Dup, PushInteger 10, Larger], While]


main :: Prelude.IO ()
main = Prelude.putStrLn $ Prelude.show $ Prelude.snd (runState (execute program) [])

