module Main where

import Prelude
import qualified Data.Stack
import Control.Monad.State

import AbstractSyntax
import Interpreter

program :: Commands
program = [PushInteger 1, PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushFunction [Dup, PushInteger 10, Larger], While]

program1 :: Commands
program1 = [PrintStr "start=", Flush, ReadCh, PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushFunction [Dup, PushInteger 10, Larger], While]

program2 :: Commands
program2 = [PushFunction [PrintStr "start=", Flush, ReadCh], PushVaradr 'q', AssignVar,
            PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushVaradr 'b', AssignVar,
            PushFunction [Dup, PushInteger 10, Larger], PushVaradr 'c', AssignVar,
            PushFunction [PushVaradr 'q', PushVar, RunFunction, PushVaradr 'b', PushVar, PushVaradr 'c', PushVar, While], PushVaradr 'm', AssignVar,
            PushVaradr 'm', PushVar, RunFunction]

--main :: Prelude.IO ()
main = runStateT (execute program2) emptyUniverse

