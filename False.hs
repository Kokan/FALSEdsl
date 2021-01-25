{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, OverloadedStrings, OverloadedLists, TypeFamilies, OverloadedStrings, OverloadedLists #-}

module False where

import qualified Prelude
import Data.Char
import Data.String
import Data.Text
import GHC.Exts
import Text.Printf

import AbstractSyntax
import AbstractSyntaxTypeClasses

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

ao :: Command
ao = Pick

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

-- The original is _ -> mm
mm :: Command
mm = Minus

(!) :: Command
(!) = RunFunction

(\@) :: Command
(\@) = Rot

(?) :: Command
(?) = If

(\~) :: Command
(\~) = Not

(\:) :: Command
(\:) = AssignVar

(.) :: Command
(.) = PrintNum

cc :: Command
cc = PrintCh

(^) :: Command
(^) = ReadCh

-- The original is Beta -> ab
ab :: Command
ab = Flush

-- The original is ; -> SC
sc :: Command
sc = PushVar

-- variables
-- abcdefghijklmnopqrstuvzwx
a :: Command
a = Push (Varadr 'a')
b :: Command
b = Push (Varadr 'b')
c :: Command
c = Push (Varadr 'c')
d :: Command
d = Push (Varadr 'd')
e :: Command
e = Push (Varadr 'e')
f :: Command
f = Push (Varadr 'f')
g :: Command
g = Push (Varadr 'g')
h :: Command
h = Push (Varadr 'h')
i :: Command
i = Push (Varadr 'i')
j :: Command
j = Push (Varadr 'j')
k :: Command
k = Push (Varadr 'k')
l :: Command
l = Push (Varadr 'l')
m :: Command
m = Push (Varadr 'm')
n :: Command
n = Push (Varadr 'n')
o :: Command
o = Push (Varadr 'o')
p :: Command
p = Push (Varadr 'p')
q :: Command
q = Push (Varadr 'q')
r :: Command
r = Push (Varadr 'r')
s :: Command
s = Push (Varadr 's')
t :: Command
t = Push (Varadr 't')
u :: Command
u = Push (Varadr 'u')
v :: Command
v = Push (Varadr 'v')
z :: Command
z = Push (Varadr 'z')
w :: Command
w = Push (Varadr 'w')
x :: Command
x = Push (Varadr 'x')

--{ faculty program in false! }
-- 
-- [$1=$[\%1\]?~[$1-f;!*]?]f:          { fac() in false }
-- 
-- "calculate the faculty of [1..8]: "
-- ab^ab'0-$$0>~\8>|$
-- "result: "
-- ~[\f;!.]?
-- ["illegal input!"]?"
-- "


fac :: Commands
fac = [ [($), 1, (==), ($), [ (\\), (%), 1, (\\) ], (?), (\~), [ ($), 1, (-), f, sc, (!), (*) ], (?) ], f, (\:),
        "calculate the faculty of [1..8]: ",
        ab, (^), ab, PushChar '0', (-), ($), ($), 0, (>), (\~), (\\), 8, (>), (\|), ($),
        "result ",
        (\~), [ (\\), f, sc, (!), (.) ], (?),
        ["illegal input"], (?), 10, cc ]

