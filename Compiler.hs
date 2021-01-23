module Compiler where

import AbstractSyntax
import Control.Monad.State

type Code = (Int, Bool)

type MyState a = State Code a

emptyState :: Bool -> Code
emptyState debug = (0, debug)

getDebug :: MyState Bool
getDebug = do
         (_,debug) <- get
         return $ debug

compileToFile :: Commands -> Bool -> FilePath -> IO ()
compileToFile cmd debug file = writeFile file $ compile cmd debug

compile :: Commands -> Bool -> String
compile c d = let (functions, main)= fst (runState (compile' c) (emptyState d))
              in  "#include <iostream>\n#include <map>\n#include \"falselib.hpp\"\n\n" <> functions <> "\n\nint main()\n{\nUniverse universe;\nstd::map<SEP,SEP> variables;\n" <> main <> "return 0;\n}"

generate_debug :: Command -> String
generate_debug c = " debug_universe(universe);\nstd::cout << R\"(" <> show c <> ")\" << std::endl;\n"

ret_body :: String -> MyState (String, String)
ret_body str = return $ ("", str)

compile' :: Commands -> MyState (String, String)
compile' [] = return $ mempty
compile' (x:xs) = do
           (func, command) <- compileCommand x
           (funcs, commands) <- compile' xs
           d <- getDebug
           if d
           then
              return $ (funcs <> "\n" <> func, command <> generate_debug x <> "\n" <> commands)
           else
              return $ (funcs <> "\n" <> func, command <>                     "\n" <> commands)
          

op2 :: String -> (String, String)
op2 f = ("", "{\nSEP b = pop(universe);\nSEP a = pop(universe);\n\npush(universe, make_integer(a->getAsInt() " <> f <> " b->getAsInt()));\n};")

op2l :: String -> (String, String)
op2l f = ("", "{\nSEP b = pop(universe);\nSEP a = pop(universe);\n\npush(universe, make_integer(a->getAsInt() " <> f <> " b->getAsInt() ? (-1) : 0 ));\n};")

op1 :: String -> (String, String)
op1 f = ("", "{\nSEP a = pop(universe);\npush(universe, make_integer(" <> f <> " a->getAsInt()));\n};")

gen_fun' :: String -> String -> String
gen_fun' name body = "void " <> name <> "(Universe &universe)\n{\n" <> body <> "\n};"

gen_fun :: String -> MyState String
gen_fun body = do
        name <- get_fun_name
        modify (\(x,y) -> (x+1,y))
        return $ gen_fun' name body

get_fun_name :: MyState String
get_fun_name = do
          (idx,_) <- get
          return $ "lambda" <> show idx

compileCommand :: Command -> MyState (String, String)
compileCommand (Push (Integer i)) = ret_body $ "push(universe, make_integer(" <> show i <> "));"
compileCommand (Push (Function f)) = do
                          (rec_fun, fun) <- compile' f

                          fun_name <- get_fun_name
                          fun <- gen_fun fun

                          return $ (rec_fun <> "\n" <> fun, "push(universe, make_function( std::function<void(Universe&)>(" <> fun_name <> ")));")
compileCommand (Push (Char c)) = ret_body $ "push(universe, make_char('" <> [c] <> "'));"
compileCommand (Push (Varadr c)) = ret_body $ "push(universe, make_varadr('" <> [c] <> "'));"

compileCommand (PushFunction c) = compileCommand (Push (Function c))
compileCommand (PushVaradr c) = compileCommand (Push (Varadr c))
compileCommand (PushInteger x) = compileCommand (Push (Integer x))
compileCommand (PushChar x) = compileCommand (Push (Char x))

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

compileCommand (PrintStr str) = ret_body $ "std::cout << \"" <> str <> "\";\n"

compileCommand command = ret_body $ show command <> "_command(universe);"
