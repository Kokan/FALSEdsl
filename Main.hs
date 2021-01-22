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
program1 = [PrintStr "start=", Flush, PushInteger 48, ReadCh, Swap, Sub, PushFunction [Dup, PushInteger 10, Swap, Larger], PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], While]

--   ß[^$1_=~][,]#		{ while c:=getc()<>EOF do putc(c) }
cp :: Commands
cp = [ Flush, PushFunction [ ReadCh, Dup, PushInteger 1, Minus, Equal, Not ], PushFunction [ PrintCh ], While ]

program2 :: Commands
program2 = [PushFunction [PrintStr "start=", Flush, PushInteger 48, ReadCh, Swap, Sub], PushVaradr 'q', AssignVar,
            PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushVaradr 'b', AssignVar,
            PushFunction [Dup, PushInteger 10, Swap, Larger], PushVaradr 'c', AssignVar,
            PushFunction [PushVaradr 'q', PushVar, RunFunction, PushVaradr 'c', PushVar, PushVaradr 'b', PushVar, While], PushVaradr 'm', AssignVar,
            PushVaradr 'm', PushVar, RunFunction]

program3 :: Commands
program3 = [PushFunction [PushFunction [PrintStr "start=", Flush, PushInteger 48, ReadCh, Swap, Sub], PushVaradr 'q', AssignVar,
            PushFunction [Dup, PrintNum, Dup, PushInteger 1, Add], PushVaradr 'b', AssignVar,
            PushFunction [Dup, PushInteger 10, Swap, Larger], PushVaradr 'c', AssignVar,
            PushFunction [PushVaradr 'q', PushVar, RunFunction, PushVaradr 'c', PushVar, PushVaradr 'b', PushVar, While], PushVaradr 'm', AssignVar,
            PushVaradr 'm', PushVar, RunFunction], RunFunction]

-- List primes up to 99
-- 99 9[1-$][\$@$@$@$@\/*=[1-$$[%\1-$@]?0=[\$.' ,\]?]?]#
primes :: Commands
primes = [ PushInteger 99, PushInteger 9,
           PushFunction [ PushInteger 1, Sub, Dup ],
           PushFunction [ Swap, Dup, Rot, Dup, Rot,  Dup, Rot, Dup, Rot, Swap, Div, Mul, Equal,
                          PushFunction [ PushInteger 1, Sub, Dup, Dup,
                                         PushFunction [ Del, Swap, PushInteger 1, Sub, Dup, Rot], If, PushInteger 0, Equal,
                                         PushFunction [ Swap, Dup, PrintNum, PrintStr " " , Swap ], If ], If ], While]

-- factorial: [$1=$[\%1\]?~[$1-f;!*]?]f:5f;!
factorial :: Commands
factorial = [ PushFunction [ Dup, PushInteger 1, Equal, Dup, PushFunction [ Swap, Del, PushInteger 1, Swap ], If, Not, PushFunction [ Dup, PushInteger 1, Sub, PushVaradr 'f', PushVar, RunFunction, Mul ], If ], PushVaradr 'f', AssignVar,
              PushInteger 5, PushVaradr 'f', PushVar, RunFunction, PrintNum ]

-- tafels: 0[1+$11\>][0[1+$11\>][1ø."x"$."="$2ø*." "]#10,%]#
tafels :: Commands
tafels = [ PushInteger 0,
           PushFunction [ PushInteger 1,  Add,  Dup,  PushInteger 11,  Swap,  Larger],
           PushFunction [ PushInteger 0,
                          PushFunction [ PushInteger 1,  Add,  Dup,  PushInteger 11,  Swap,  Larger],
                          PushFunction [ PushInteger 1, Pick,  PrintNum,  PrintStr "x",  Dup,  PrintNum,  PrintStr "=",  Dup,  PushInteger 2, Pick,  Mul,  PrintNum,  PrintStr " "],
                          While,
                          PushInteger 10,  PrintCh,  Del],
           While ]

pickTest :: Commands
pickTest = [ PushInteger 3, PushInteger 2, PushInteger 1, PushInteger 4, Pick, PrintNum ]

beer :: Commands
beer = [ 
  PushFunction [ Dup,  PushInteger 0,  Equal,
                 PushFunction [ PrintStr "no more bottles"],
                 If,  Dup,  PushInteger 1,  Equal,
                 PushFunction [ PrintStr "One bottle"],
                 If,  Dup,  PushInteger 1,  Larger,
                 PushFunction [ Dup,  PrintNum,  PrintStr " bottles"],
                 If,  Del,  PrintStr " of beer"],
   PushChar 'b',  AssignVar ,  PushInteger 100,
   PushFunction [ Dup,  PushInteger 0,  Larger],
   PushFunction [ Dup,  PushChar 'b',  PushVar,  RunFunction,  PrintStr " on the wall, ",  Dup,  PushChar 'b',  PushVar,  RunFunction,  PrintStr ".\\n",  PushInteger 1,  Sub,  PrintStr "Take one down, pass it around, ",  Dup,  PushChar 'b',  PushVar,  RunFunction,  PrintStr " on the wall.\\n"],  While,  Del ]

-- [[$' =][%^]#]b:
-- [$$'.=\' =|~]w:
-- [$'.=~[' ,]?]s:
-- [w;![^o;!\,]?]o:
-- ^b;![$'.=~][w;[,^]#b;!s;!o;!b;!s;!]#,
oddword :: Commands
oddword = [  PushFunction [ PushFunction [ Dup, PushChar ' ',  Equal],  PushFunction [ Del,  PrintCh],  While],  PushChar 'b',  AssignVar,
             PushFunction [ Dup,  Dup, PushChar ' ',  PrintNum,  Equal,  Swap, PushChar ' ',  Equal,  Or,  Not],  PushChar 'w',  AssignVar,
             PushFunction [ Dup, PushChar ' ',  PrintNum,  Equal,  Not,  PushFunction [ PushChar ' ',  PrintCh],  If],  PushChar 's',  AssignVar,
             PushFunction [ PushChar 'w',  PushVar,  RunFunction,  PushFunction [ PrintCh,  PushChar 'o',  PushVar,  RunFunction,  Swap,  PrintCh],  If],  PushChar 'o',  AssignVar,
             ReadCh, PushChar 'b',  PushVar,  RunFunction,  PushFunction [ Dup, PushChar ' ',  PrintNum,  Equal,  Not],  PushFunction [ PushChar 'w',  PushVar,  PushFunction [ ReadCh,  PrintCh],  While,  PushChar 'b',  PushVar,  RunFunction,  PushChar 's',  PushVar,  RunFunction,  PushChar 'o',  PushVar,  RunFunction,  PushChar 'b',  PushVar,  RunFunction,  PushChar 's',  PushVar,  RunFunction],  While,  PrintCh ]

currentProgram :: Commands
currentProgram = program1

main = runStateT (execute currentProgram) emptyUniverse
--main = compileToFile currentProgram "test.cpp"

