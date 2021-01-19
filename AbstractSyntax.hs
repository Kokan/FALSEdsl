{-# LANGUAGE GADTs #-}
module AbstractSyntax where

type Label = Char

data StackEntry where
    Integer  :: Int -> StackEntry
    Varadr   :: Label -> StackEntry
    Char     :: Char -> StackEntry
    Function :: Commands -> StackEntry
    deriving (Show)


data Command where
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


ap2StackEntry :: (Int -> Int -> Int) -> StackEntry -> StackEntry -> StackEntry
ap2StackEntry f (Integer x) (Integer y) = Integer (x `f` y)
ap2StackEntry _ _ _ = error "not supported operation"

ap1StackEntry :: (Int -> Int) -> StackEntry -> StackEntry
ap1StackEntry f (Integer x) = Integer (f x)
ap1StackEntry _ _ = error "not supported operation"

apStackEntryEqual :: StackEntry -> StackEntry -> StackEntry
apStackEntryEqual (Integer x) (Integer y) | x == y    = Integer (-1)
                                          | otherwise = Integer 0
apStackEntryEqual (Char x) (Char y) | x == y    = Integer (-1)
                                    | otherwise = Integer 0
apStackEntryEqual _ _ = error "not supported operation"

apStackEntryLarger :: StackEntry -> StackEntry -> StackEntry
apStackEntryLarger (Integer x) (Integer y) | x > y     = Integer (-1)
                                           | otherwise = Integer 0
apStackEntryLarger (Char x) (Char y) | x > y     = Integer (-1)
                                     | otherwise = Integer 0
apStackEntryLarger _ _ = error "not supported operation"


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
ifStackEntry _ = error "not supported operation"

stackEntry2int :: StackEntry -> Int
stackEntry2int (Integer x) = x
stackEntry2int _ = error "not supported operation"


