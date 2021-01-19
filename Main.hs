module Main where

import Prelude
import qualified Data.Stack
import Control.Monad.State

import AbstractSyntax
import Interpreter
import Compiler

dummy :: Commands
dummy = [PushInteger 0, PushVaradr 'a', AssignVar, PrintStr "please provide a number: ", Flush, ReadCh, PushVaradr 'a', PushVar, Swap, Sub,  PrintNum]

program :: Commands
program = [PushInteger 1, PushFunction [Dup, PushInteger 10, Swap, Larger], PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], While]

larger :: Commands
larger = [ PushInteger 3, PushInteger 2, Larger, PrintNum ]

program1 :: Commands
program1 = [PrintStr "start=", Flush, PushInteger 48, ReadCh, Sub, PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushFunction [Dup, PushInteger 10, Larger], While]

program2 :: Commands
program2 = [PushFunction [PrintStr "start=", Flush, PushInteger 48, ReadCh, Sub], PushVaradr 'q', AssignVar,
            PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushVaradr 'b', AssignVar,
            PushFunction [Dup, PushInteger 10, Larger], PushVaradr 'c', AssignVar,
            PushFunction [PushVaradr 'q', PushVar, RunFunction, PushVaradr 'b', PushVar, PushVaradr 'c', PushVar, While], PushVaradr 'm', AssignVar,
            PushVaradr 'm', PushVar, RunFunction]

program3 :: Commands
program3 = [PushFunction [PushFunction [PrintStr "start=", Flush, PushInteger 48, ReadCh, Sub], PushVaradr 'q', AssignVar,
            PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushVaradr 'b', AssignVar,
            PushFunction [Dup, PushInteger 10, Larger], PushVaradr 'c', AssignVar,
            PushFunction [PushVaradr 'q', PushVar, RunFunction, PushVaradr 'b', PushVar, PushVaradr 'c', PushVar, While], PushVaradr 'm', AssignVar,
            PushVaradr 'm', PushVar, RunFunction], RunFunction]

-- List primes up to 99
-- 99 9[1-$]
--     [$@$@$@$@/*=
--        [1-$$
--           [%1-$@]
--         ?0
--           [$.' ,]
--         ?
--        ]?
--      ]#
primes :: Commands
primes = [  PushInteger 99, PushInteger 9, PushFunction [ PushInteger 1, Sub, Dup ], PushFunction [ Dup, Rot, Dup, Rot, Dup, Rot, Dup, Rot, Div, Mul, Equal, PushFunction [ PushInteger 1, Sub, Dup, Dup, PushFunction [ Del, PushInteger 1, Sub, Dup, Rot ], If, PushInteger 1, PushFunction [ Dup, PrintNum, PushChar ' ', PrintCh ], If ], If ], While ]

-- factorial: [$1=$[\%1\]?~[$1-f;!*]?]f:5f;!
factorial :: Commands
factorial = [ PushFunction [ Dup, PushInteger 1, Equal, Dup, PushFunction [ Swap, Del, PushInteger 1, Swap ], If, Not, PushFunction [ Dup, PushInteger 1, Sub, PushVaradr 'f', PushVar, RunFunction, Mul ], If ], PushVaradr 'f', AssignVar,
              PushInteger 5, PushVaradr 'f', PushVar, RunFunction, PrintNum ]

pickTest :: Commands
pickTest = [ PushInteger 3, PushInteger 2, PushInteger 1, PushInteger 4, Pick, PrintNum ]

currentProgram :: Commands
currentProgram = larger

--main = runStateT (execute currentProgram) emptyUniverse
main = compileToFile currentProgram "test.cpp"

