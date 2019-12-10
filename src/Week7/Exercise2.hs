
--from the assignment:
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
--

module Week7.Exercise2 where

import Control.Applicative (Const)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Classes
-- Mitä helvetin pattern synonyymejä?


newtype Fix m = Fix {unFix :: m (Fix m)}

type Bool' = Fix (Bool'') 
data Bool'' r = T | F

instance Show1 m => Show (Fix m) where
  showsPrec n (Fix x) = showParen (n >= 11)
    (showString "Fix " . showsPrec1 11 x)

$(deriveShow1 ''Bool'') -- template haskell splice


data Maybe' a = M (Fix (Const (Maybe' a)))
data Maybe'' a = N | J a

-- data Either' a b = Either' a b --------------------------TODO

type U' = Fix (Const U'')
data U'' = U

-- how does the a go to the L'' here? ... --------------------------ASK?
-- Show?  --------------------------TODO?
data L' a = L (Fix (L'')) -- deriving (Show)
data L'' a = L'' a | Empti -- deriving (Show)


-- ... Void = ... --------------------------TODO

-- ... Identity a = --- --------------------------TODO


