{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables #-}

module False where

import qualified Prelude

import AbstractSyntax

--instance Num (Elem Int) where
--    a + b = App (App (Symbol "+" (+)) a) b
--    a - b = App (App (Symbol "-"(-)) a) b
--    a * b = App (App (Symbol "*" (*)) a) b
--    negate a = App (Symbol "negate" negate) a
--    signum a = App (Symbol "signum" signum) a
--    abs a = App (Symbol "abs" abs) a
--    fromInteger a = Symbol (show a) (fromInteger a)

($) :: Command
($) = Dup

(+) :: Command
(+) = Add

-- The original is =, but that cannot be changed in Haskell
-- using == instead.
(==) :: Command
(==) = Equal

(>) :: Command
(>) = Larger

(\\) :: Command
(\\) = Swap

(%) :: Command
(%) = Del

(/) :: Command
(/) = Div

(&) :: Command
(&) = And

(\|) :: Command
(\|) = Or

(*) :: Command
(*) = Mul

(-) :: Command
(-) = Sub

(#) :: Command
(#) = While

-- The original is _ -> m
m :: Command
m = Minus

(!) :: Command
(!) = RunFunction

(?) :: Command
(?) = If

(\~) :: Command
(\~) = Not

(\:) :: Command
(\:) = AssignVar

(.) :: Command
(.) = PrintNum

c :: Command
c = PrintCh

(^) :: Command
(^) = ReadCh

-- The original is Beta -> b
b :: Command
b = Flush

-- The original is ; -> SC
sc :: Command
sc = PushVar

--{ faculty program in false! }
-- 
-- [$1=$[\%1\]?~[$1-f;!*]?]f:          { fac() in false }
-- 
-- "calculate the faculty of [1..8]: "
-- b^b'0-$$0>~\8>|$
-- "result: "
-- ~[\f;!.]?
-- ["illegal input!"]?"
-- "


fac :: Commands
fac = [ PushFunction [($), PushInteger 1, (==), ($), PushFunction [ (\\), (%), PushInteger 1, (\\) ], (?), (\~), PushFunction [ ($), PushInteger 1, (-), PushVaradr 'f', sc, (!), (*) ], (?) ], PushVaradr 'f', (\:),
        PrintStr "calculate the faculty of [1..8]: ",
        b, (^), b, PushChar '0', (-), ($), ($), PushInteger 0, (>), (\~), (\\), PushInteger 8, (>), (\|), ($),
        PrintStr "result ",
        (\~), PushFunction [ (\\), PushVaradr 'f', sc, (!), (.) ], (?),
        PushFunction [PrintStr "illegal input"], (?), PushInteger 10, c ]

