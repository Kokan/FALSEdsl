module Compiler where

import AbstractSyntax
import Control.Monad.State

type MyState a = State String a

compile :: Commands -> MyState String
compile [] = do
          universe <- get
          return universe
compile (x:xs) = do
           compileCommand x
           res <- get
           put $ res <> "\n"
           compile xs

op2 :: String -> String
op2 f = "{\nint a = stack.pop();\n int b = stack.pop();\nstack.push(a " <> f <> " b);\n};"

op1 :: String -> String
op1 f = "{\nint a = stack.pop();\nstack.push(" <> f <> " a);\n};"

compileCommand :: Command -> MyState ()
compileCommand (PushFunction c) = do
                          fun <- compile c
                          put $ "stack.push(" <> fun <> ");"
compileCommand (PushVaradr c) = put $ "stack.push(" <> [c] <> ");"
compileCommand (PushInteger x) = put $ "stack.push(" <> show x <> ");"
compileCommand (PushChar x) = put $ "stack.push(" <> [x] <> ");"

compileCommand (AssignVar) = undefined
compileCommand (PushVar) = undefined
compileCommand (RunFunction) = undefined

compileCommand (Add) = put $ op2 "+"
compileCommand (Sub) = put $ op2 "-"
compileCommand (Mul) = put $ op2 "*"
compileCommand (Div) = put $ op2 "/"
compileCommand (Minus) = put $ op1 "-"

compileCommand (Equal) = put $ op2 "=="
compileCommand (Larger) = put $ op2 ">"

compileCommand (And) = put $ op2 "&&"
compileCommand (Or) = put $ op2 "||"
compileCommand (Not) = put $ op1 "!"

compileCommand (If) = put $ "{\nint a = stack.pop();\nif (a)\n{\n}\n}"
compileCommand (While) = undefined

compileCommand (Dup) = put $ "{\nint a = stack.pop(); stack.push(a);\nstack.push(a);\n}\n"
compileCommand (Del) = put $ "stack.pop();"
compileCommand (Swap) = put $ "{\nint a = stack.pop();\n int b = stack.pop();\nstack.push(b);\nstack.push(b);\n};"
compileCommand (Rot) = undefined
compileCommand (Pick) = undefined

compileCommand (PrintNum) = put $ "{\nint a = stack.pop(); std::cout << a;\n}"
compileCommand (PrintStr str) = put $ "std::cout << \"" <> str <> "\";\n"
compileCommand (PrintCh) = put $ "{\nint a = stack.pop(); std::cout << a;\n}"
compileCommand (ReadCh) = undefined
compileCommand (Flush) = put $ "std::flush(std::cout);"

