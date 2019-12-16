{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Week7.Exercise2 where

import Control.Applicative (Const)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Classes
import Data.Stream.Infinite
import Data.Tree

newtype Fix m = Fix {unFix :: m (Fix m)}

type Bool' = Fix (Bool'') 
data Bool'' r = T | F

instance Show1 m => Show (Fix m) where
  showsPrec n (Fix x) = showParen (n >= 11)
    (showString "Fix " . showsPrec1 11 x)

$(deriveShow1 ''Bool'') -- template haskell splice

data M' a = M' (Fix (M'' a))
data M'' a r = N' | J' a

$(deriveShow1 ''M'') -- template haskell splice
pattern N = M' (Fix N')
pattern J a = M' (Fix (J' a))

-- data Either' a b = Either' a b --------------------------TODO

--unit
type U' = Fix U''
data U'' r = U'''

$(deriveShow1 ''U'')
pattern U = Fix U'''

type L' a = (Fix (L'' a)) -- deriving (Show)
data L'' a r = L'' a r | Empti deriving (Show)

$(deriveShow1 ''L'')
pattern L x xs = (Fix (L'' x xs))
pattern E =(Fix (Empti)) 


type Void' = (Fix Void'')
data Void'' r 

-- data Identity a = 

type Streem' a = Fix (Streem'' a)
data Streem'' a r = a ::> r

$(deriveShow1 ''Streem'')
pattern Streem x xs = Fix (x ::> xs)


type Tree' a = Fix (Tree'' a)
data Tree'' a r = Node' a [r]
-- data Forest' r = Forest' r | AllTreesCut deriving (Show)

$(deriveShow1 ''Tree'')
-- $(deriveShow1 ''Forest')

pattern Tr x = Fix x
pattern Tr' x xs = Fix (Node' x (xs))
-- pattern Fr left right = (Forest' left right)





