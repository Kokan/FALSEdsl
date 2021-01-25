{-# LANGUAGE FlexibleInstances, AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, OverloadedStrings, OverloadedLists, TypeFamilies, OverloadedStrings, OverloadedLists #-}
module AbstractSyntaxTypeClasses where

import Prelude
import Data.Char
import GHC.Exts
import Text.Printf

import AbstractSyntax

instance Num Command where
     (+) = ap2Command (+)
     (-) = ap2Command (-)
     (*) = ap2Command (*)
     abs = ap1Command (abs)
     signum = ap1Command (signum)
     fromInteger a = Push (fromInteger a)


instance Num StackEntry where
     (+) = ap2StackEntry (+)
     (-) = ap2StackEntry (-)
     (*) = ap2StackEntry (*)
     abs = ap1StackEntry (abs)
     signum = ap1StackEntry (signum)
     fromInteger a = Integer (fromInteger a)

instance IsString Command where
    fromString str = PrintStr str

instance IsList Command where
    type Item Command = Command
    fromList xs = Push $ Function xs
    toList (Push (Function xs)) = xs
    toList _ = error "unsupported"

