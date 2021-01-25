{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, OverloadedStrings, OverloadedLists, TypeFamilies, OverloadedStrings, OverloadedLists #-}
module Examples where
 
import qualified Prelude
import AbstractSyntax
import AbstractSyntaxTypeClasses
import False

-- 1a:"please provide a number: "ø^a;+,
dummy :: Commands
dummy = [1, a, (\:), "please provide a char: ", ab, (^), a, sc, (+),  cc]

-- 1[$10\>][$.$1+]#
print1to9 :: Commands
print1to9 = [1, [($), 10, (\\), (>)], [($), (.), ($), 1, (+)], (#)]

-- "start="ß48^\-[$10\>][$.$1+]#
printnto9 :: Commands
printnto9 = ["start=", ab, 48, (^), (\\), (-), [($), 10, (\\), (>)], [($), (.), ($), 1, (+)], (#)]

--   ß[^$1_=~][,]#		{ while c:=getc()<>EOF do putc(c) }
cp :: Commands
cp = [ ab, [ (^), ($), 1, mm, (==), (\~) ], [ cc ], (#) ]

print1to9_1 :: Commands
print1to9_1 = [["start=", ab, 48, (^), (\\), (-)], q, (\:),
            [($), (.), ($), 1, (+)], b, (\:),
            [($), 10, (\\), (>)], c, (\:),
            [q, sc, (!), c, sc, b, sc, (#)], m, (\:),
            m, sc, (!)]

print1to9_2 :: Commands
print1to9_2 = [[["start=", ab, 48, (^), (\\), (-)], q, (\:),
            [($), (.), ($), 1, (+)], b, (\:),
            [($), 10, (\\), (>)], c, (\:),
            [q, sc, (!), c, sc, b, sc, (#)], m, (\:),
            m, sc, (!)], (!)]

-- List primes up to 1000
-- 1000 9[1-$][\$@$@$@$@\/*=[1-$$[%\1-$@]?0=[\$.' ,\]?]?]#
primes :: Commands
primes = [ 1000, 9,
           [ 1, (-), ($) ],
           [ (\\), ($), (\@), ($), (\@),  ($), (\@), ($), (\@), (\\), (/), (*), (==),
                          [ 1, (-), ($), ($),
                                         [ (%), (\\), 1, (-), ($), (\@)], (?), 0, (==),
                                         [ (\\), ($), (.), " " , (\\) ], (?) ], (?) ], (#)]

-- factorial: [$1=$[\%1\]?~[$1-f;!*]?]f:5f;!
factorial :: Commands
factorial = [ [ ($), 1, (==), ($), [ (\\), (%), 1, (\\) ], (?), (\~), [ ($), 1, (-), f, sc, (!), (*) ], (?) ], f, (\:),
              5, f, sc, (!), (.) ]

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

-- tafels: 0[1+$11\>][0[1+$11\>][1ø."x"$."="$2ø*." "]#10,%]#
tafels :: Commands
tafels = [ 0,
           [ 1,  (+),  ($),  11,  (\\),  (>)],
           [ 0,
                          [ 1,  (+),  ($),  11,  (\\),  (>)],
                          [ 1, ao,  (.),  "x",  ($),  (.),  "=",  ($),  2, ao,  (*),  (.),  " "],
                          (#),
                          10,  cc,  (%)],
           (#) ]


--{ False version of 99 Bottles by Marcus Comstedt (marcus@lysator.liu.se) }
--[$0=["no more bottles"]?$1=["One bottle"]?$1>[$." bottles"]?%" of beer"]b:
--100[$0>][$b;!" on the wall, "$b;!".
--"1-"Take one down, pass it around, "$b;!" on the wall.
--"]#%

beer :: Commands
beer = [ 
  [ ($),  0,  (==),
                 [ "no more bottles"],
                 (?),  ($),  1,  (==),
                 [ "One bottle"],
                 (?),  ($),  1,  (>),
                 [ ($),  (.),  " bottles"],
                 (?),  (%),  " of beer"],
   b,  (\:) ,  100,
   [ ($),  0,  (>)],
   [ ($),  b,  sc,  (!),  " on the wall, ",  ($),  b,  sc,  (!),  ".", 10, cc,  1,  (-),  "Take one down, pass it around, ",  ($),  b,  sc,  (!),  " on the wall.", 10, cc],  (#),  (%) ]


