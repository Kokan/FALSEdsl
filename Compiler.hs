module Compiler where

import AbstractSyntax
import Control.Monad.State

type Code = (Int)

type MyState a = State Code a

emptyState :: Code
emptyState = (0)

compileToFile :: Commands -> FilePath -> IO ()
compileToFile c file = writeFile file $ compile c

compile :: Commands -> String
compile c = let result = fst (runState (compile' c) emptyState)
            in  "#include <iostream>\n#include <map>\n#include \"falselib.hpp\"\n\n" <> fst result <> "\n\nint main()\n{\nUniverse universe;\nstd::map<SEP,SEP> variables;\n" <> snd result <> "return 0;\n}"

debug :: Command -> String
--debug c = "\ndebug_universe(universe);\nstd::cout << R\"(" <> show c <> ")\" << std::endl;\n"
debug c = ""

compile' :: Commands -> MyState (String, String)
compile' [] = return $ mempty
compile' (x:xs) = do
           (func, command) <- compileCommand x
           (funcs, commands) <- compile' xs
           return $ (funcs <> "\n" <> func, command <> debug x <> "\n" <> commands)

op2 :: String -> (String, String)
op2 f = ("", "{\nSEP b = pop(universe);\nSEP a = pop(universe);\n\npush(universe, make_integer(a->getAsInt() " <> f <> " b->getAsInt()));\n};")

op2l :: String -> (String, String)
op2l f = ("", "{\nSEP b = pop(universe);\nSEP a = pop(universe);\n\npush(universe, make_integer(a->getAsInt() " <> f <> " b->getAsInt() ? (-1) : 0 ));\n};")

op1 :: String -> (String, String)
op1 f = ("", "{\nSEP a = pop(universe);\npush(universe, make_integer(" <> f <> " a->getAsInt()));\n};")

gen_fun :: String -> MyState String
gen_fun fun = do
        name <- get_fun_name
        modify (+1)
        return $ "void " <> name <> "(Universe &universe)\n{\n" <> fun <> "\n};"

get_fun_name :: MyState String
get_fun_name = do
          idx <- get
          return $ "gen_fun" <> show idx

compileCommand :: Command -> MyState (String, String)
compileCommand (PushFunction c) = do
                          (rec_fun, fun) <- compile' c
                          fun_name <- get_fun_name
                          fun <- gen_fun fun
                          return $ (rec_fun <> "\n" <> fun, "push(universe, make_function( std::function<void(Universe&)>(" <> fun_name <> ")));")
compileCommand (PushVaradr c) = return $ ("", "push(universe, make_varadr('" <> [c] <> "'));")
compileCommand (PushInteger x) = return $ ("", "push(universe, make_integer(" <> show x <> "));")
compileCommand (PushChar x) = return $ ("", "push(universe, make_char('" <> [x] <> "'));")

compileCommand (AssignVar) = return $ ("", "assign_var_command(universe);")
compileCommand (PushVar) = return $ ("", "push_var_command(universe);")
compileCommand (RunFunction) = return $ ("", "{ SEP f = pop(universe);\nf->call(universe);\n}")

compileCommand (Add) = return $ op2 "+"
compileCommand (Sub) = return $ op2 "-"
compileCommand (Mul) = return $ op2 "*"
compileCommand (Div) = return $ op2 "/"
compileCommand (Minus) = return $ op1 "-"

compileCommand (Equal) = return $ op2l "=="
compileCommand (Larger) = return $ op2l ">"

compileCommand (And) = return $ op2 "&&"
compileCommand (Or) = return $ op2 "||"
compileCommand (Not) = return $ op1 "!"

compileCommand (If) = return $ ("", "{\nSEP comm = pop(universe);\nSEP cond = pop(universe);\nif (cond->getAsInt())\n{\ncomm->call(universe);\n}\n}")
compileCommand (While) = return $ ("", "while_command(universe);")


compileCommand (Dup) = return $ ("", "{\nSEP a = pop(universe);\nSEP cloned = clone(a.get());\npush(universe, std::move(a));\npush(universe, std::move(cloned));\n}\n")
compileCommand (Del) = return $ ("", "pop(universe);")
compileCommand (Swap) = return $ ("", "{\nSEP a = pop(universe);\nSEP b = pop(universe);\npush(universe, std::move(a));\npush(universe, std::move(b));\n};")
compileCommand (Rot) = return $ ("", "rotate_command(universe);")
compileCommand (Pick) = return $ ("", "pick_command(universe);")

compileCommand (PrintNum) = return $ ("", "{\nSEP a = pop(universe);\nstd::cout << a->getAsInt();\n}")
compileCommand (PrintStr str) = return $ ("", "std::cout << \"" <> str <> "\";\n")
compileCommand (PrintCh) = return $ ("", "{\nSEP a = pop(universe);\nstd::cout << a->getAsChar();\n}")
compileCommand (ReadCh) = return $ ("", "{\nchar a;\nstd::cin >> a;\npush(universe, make_char(a));\n}")
compileCommand (Flush) = return $ ("", "std::flush(std::cout);")

