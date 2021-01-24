{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, OverloadedStrings, OverloadedLists, TypeFamilies, OverloadedStrings, OverloadedLists #-}
module Examples where
 
import qualified Prelude
import AbstractSyntax
import AbstractSyntaxTypeClasses
import False

dummy :: Commands
dummy = [0, a, (\:), "please provide a number: ", bb, (^), a, sc, (\\), (-),  (.)]

program :: Commands
program = [1, [($), 10, (\\), (>)], [($), (.), ($), 1, (+)], (#)]

larger :: Commands
larger = [ 3, 2, (>), (.) ]

program1 :: Commands
program1 = ["start=", bb, 48, (^), (\\), (-), [($), 10, (\\), (>)], [($), (.), ($), 1, (+)], (#)]

--   ß[^$1_=~][,]#		{ while c:=getc()<>EOF do putc(c) }
cp :: Commands
cp = [ bb, [ (^), ($), 1, mm, (==), (\~) ], [ cc ], (#) ]

program2 :: Commands
program2 = [["start=", bb, 48, (^), (\\), (-)], q, (\:),
            [($), (.), ($), 1, (+)], b, (\:),
            [($), 10, (\\), (>)], c, (\:),
            [q, sc, (!), c, sc, b, sc, (#)], m, (\:),
            m, sc, (!)]

program3 :: Commands
program3 = [[["start=", bb, 48, (^), (\\), (-)], q, (\:),
            [($), (.), ($), 1, (+)], b, (\:),
            [($), 10, (\\), (>)], c, (\:),
            [q, sc, (!), c, sc, b, sc, (#)], m, (\:),
            m, sc, (!)], (!)]

-- List primes up to 99
-- 99 9[1-$][\$@$@$@$@\/*=[1-$$[%\1-$@]?0=[\$.' ,\]?]?]#
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

pickTest :: Commands
pickTest = [ 3, 2, 1, 4, ao, (.) ]

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
   [ ($),  b,  sc,  (!),  " on the wall, ",  ($),  b,  sc,  (!),  ".\\n",  1,  (-),  "Take one down, pass it around, ",  ($),  b,  sc,  (!),  " on the wall.\\n"],  (#),  (%) ]

-- [[$' =][%^]#]b:
-- [$$'.=\' =|~]w:
-- [$'.=~[' ,]?]s:
-- [w;![^o;!\,]?]o:
-- ^b;![$'.=~][w;[,^]#b;!s;!o;!b;!s;!]#,
oddword :: Commands
oddword = [  [ [ ($), PushChar ' ',  (==)],  [ (%),  cc],  (#)],  b,  (\:),
             [ ($),  ($), PushChar ' ',  (.),  (==),  (\\), PushChar ' ',  (==),  Or,  (\~)],  w,  (\:),
             [ ($), PushChar ' ',  (.),  (==),  (\~),  [ PushChar ' ',  cc],  (?)],  s,  (\:),
             [ w,  sc,  (!),  [ cc,  o,  sc,  (!),  (\\),  cc],  (?)],  o,  (\:),
             (^), b,  sc,  (!),  [ ($), PushChar ' ',  (.),  (==),  (\~)],  [ w,  sc,  [ (^),  cc],  (#),  b,  sc,  (!),  s,  sc,  (!),  o,  sc,  (!),  b,  sc,  (!),  s,  sc,  (!)],  (#),  cc ]

