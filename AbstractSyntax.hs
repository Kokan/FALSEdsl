{-# LANGUAGE GADTs #-}
module AbstractSyntax where

import qualified Prelude

type Label = Prelude.Char

data StackEntry where
    Integer  :: Prelude.Int -> StackEntry
    Varadr   :: Label -> StackEntry
    Char     :: Prelude.Char -> StackEntry
    Function :: Commands -> StackEntry

instance Prelude.Show StackEntry where
    show (Integer x) = "(Integer " Prelude.<> Prelude.show x Prelude.<> ")"
    show (Varadr x) = "(Varadr " Prelude.<> Prelude.show x Prelude.<> ")"
    show (Char x) = "(Char " Prelude.<> Prelude.show x Prelude.<> ")"
    show (Function x) = "(Function " Prelude.<> Prelude.show x Prelude.<> ")"


data Command where
    PushFunction :: Commands -> Command
    PushVaradr :: Prelude.Char -> Command
    PushInteger :: Prelude.Int -> Command
    PushChar :: Prelude.Char -> Command

    AssignVar:: Command
    PushVar  :: Command
    RunFunction  :: Command

    Add      :: Command
    Sub      :: Command
    Mul      :: Command
    Div      :: Command
    Minus    :: Command

    Equal    :: Command
    Larger    :: Command

    And      :: Command
    Or       :: Command
    Not      :: Command

    If       :: Command
    While    :: Command

    Dup      :: Command
    Del      :: Command
    Swap     :: Command
    Rot      :: Command
    Pick     :: Command

    PrintNum :: Command
    PrintStr :: Prelude.String -> Command
    PrintCh  :: Command
    ReadCh   :: Command
    Flush    :: Command

instance Prelude.Show Command where
    show (PushFunction c) = "PushFunction " Prelude.<> Prelude.show c
    show (PushVaradr x) = "PushVaradr " Prelude.<> Prelude.show x 
    show (PushInteger x) = "PushInteger " Prelude.<> Prelude.show x 
    show (PushChar x) = "PushChar " Prelude.<> Prelude.show x 

    show (AssignVar) = "AssignVar"
    show (PushVar) = "PushVar"
    show (RunFunction) = "RunFunction"

    show (Add) = "Add"
    show (Sub) = "Sub"
    show (Mul) = "Mul"
    show (Div) = "Div"
    show (Minus) = "Minus"

    show (Equal) = "Equal"
    show (Larger) = "Larger"

    show (And) = "And"
    show (Or) = "Or"
    show (Not) = "Not"

    show (If) = "If"
    show (While) = "While"

    show (Dup) = "Dup"
    show (Del) = "Del"
    show (Swap) = "Swap"
    show (Rot) = "Rot"
    show (Pick) = "Pick"

    show (PrintNum) = "PrintNum"
    show (PrintStr str) = "PrintStr " Prelude.<> str
    show (PrintCh) = "PrintCh"
    show (ReadCh) = "ReadCh"
    show (Flush) = "Flush"

type Commands = [Command]


ap2StackEntry :: (Prelude.Int -> Prelude.Int -> Prelude.Int) -> StackEntry -> StackEntry -> StackEntry
ap2StackEntry f (Integer x) (Integer y) = Integer (f x y)
ap2StackEntry _ _ _ = Prelude.error "not supported operation"

ap1StackEntry :: (Prelude.Int -> Prelude.Int) -> StackEntry -> StackEntry
ap1StackEntry f (Integer x) = Integer (f x)
ap1StackEntry _ _ = Prelude.error "not supported operation"

apStackEntryEqual :: StackEntry -> StackEntry -> StackEntry
apStackEntryEqual (Integer x) (Integer y) | x Prelude.== y    = Integer 1
                                          | Prelude.otherwise = Integer 0
apStackEntryEqual (Char x) (Char y) | x Prelude.== y    = Integer 1
                                    | Prelude.otherwise = Integer 0
apStackEntryEqual _ _ = Prelude.error "not supported operation"

apStackEntryLarger :: StackEntry -> StackEntry -> StackEntry
apStackEntryLarger (Integer x) (Integer y) | x Prelude.> y     = Integer 1
                                           | Prelude.otherwise = Integer 0
apStackEntryLarger (Char x) (Char y) | x Prelude.> y     = Integer 1
                                     | Prelude.otherwise = Integer 0
apStackEntryLarger _ _ = Prelude.error "not supported operation"


andInt :: Prelude.Int -> Prelude.Int -> Prelude.Int
andInt 0 _ = 0
andInt _ 0 = 0
andInt _ _ = 1


orInt :: Prelude.Int -> Prelude.Int -> Prelude.Int
orInt 0 0 = 0
orInt _ _ = 1

ifStackEntry :: StackEntry -> Prelude.Bool
ifStackEntry (Integer 0) = Prelude.False
ifStackEntry (Integer _) = Prelude.True
ifStackEntry _ = Prelude.error "not supported operation"

stackEntry2int :: StackEntry -> Prelude.Int
stackEntry2int (Integer x) = x
stackEntry2int _ = Prelude.error "not supported operation"


