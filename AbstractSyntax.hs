{-# LANGUAGE GADTs, TypeFamilies, OverloadedStrings, OverloadedLists #-}
module AbstractSyntax where

import Data.Char
import Data.String
import Data.Text
import GHC.Exts
import Text.Printf

type Label = Char

data StackEntry where
    Integer  :: Int -> StackEntry
    Varadr   :: Label -> StackEntry
    Char     :: Char -> StackEntry
    Function :: Commands -> StackEntry
--    deriving (Show)

instance Show StackEntry where
     show (Integer x) = show x
     show (Char c) = show c
     show (Varadr c) = show c
     show (Function _) = "λ"

data Command where
    Push :: StackEntry -> Command
    PushFunction :: Commands -> Command
    PushVaradr :: Char -> Command
    PushInteger :: Int -> Command
    PushChar :: Char -> Command

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
    PrintStr :: String -> Command
    PrintCh  :: Command
    ReadCh   :: Command
    Flush    :: Command
    deriving (Show)

type Commands = [Command]

ap2Command :: (Int -> Int -> Int) -> Command -> Command -> Command
ap2Command f (Push x) (Push y) = Push $ ap2StackEntry f x y
ap2Command f x y = error $ " x: " <> show x <> " y: " <> show y

ap1Command :: (Int -> Int) -> Command -> Command
ap1Command f (Push x) = Push $ ap1StackEntry f x
ap1Command f x = error $ " x: " <> show x

ap2StackEntry :: (Int -> Int -> Int) -> StackEntry -> StackEntry -> StackEntry
ap2StackEntry f x y = Integer ((stackEntry2int x) `f` (stackEntry2int y))

ap1StackEntry :: (Int -> Int) -> StackEntry -> StackEntry
ap1StackEntry f e = Integer $ f $ stackEntry2int e

apStackEntryEqual :: StackEntry -> StackEntry -> StackEntry
apStackEntryEqual x y | (stackEntry2int x) == (stackEntry2int y) = Integer (-1)
                      | otherwise = Integer 0

apStackEntryLarger :: StackEntry -> StackEntry -> StackEntry
apStackEntryLarger x y | (stackEntry2int x) > (stackEntry2int y) = Integer (-1)
                       | otherwise = Integer 0


andInt :: Int -> Int -> Int
andInt 0 _ = 0
andInt _ 0 = 0
andInt _ _ = (-1)


orInt :: Int -> Int -> Int
orInt 0 0 = 0
orInt _ _ = (-1)

ifStackEntry :: StackEntry -> Bool
ifStackEntry (Integer 0) = False
ifStackEntry (Integer _) = True
ifStackEntry _ = error "if not supported operation"

stackEntry2int :: StackEntry -> Int
stackEntry2int (Integer x) = x
stackEntry2int (Char c) = ord c
stackEntry2int _ = error "2int not supported operation"


