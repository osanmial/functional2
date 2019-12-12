{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}

module Week7.Exercise2 where

import Control.Applicative (Const)
import Text.Show.Deriving (deriveShow1)
import Data.Functor.Classes
import Data.Stream.Infinite
import Data.Tree

newtype Fix m = Fix {unFix :: m (Fix m)}



instance Show1 m => Show (Fix m) where
  showsPrec n (Fix x) = showParen (n >= 11)
    (showString "Fix " . showsPrec1 11 x)

--- Generator for Bool.
type Bool' = Fix (Bool'') 
data Bool'' r = T | F
$(deriveShow1 ''Bool'') -- template haskell splice


-- Generator for Maybe a.
data M' a = M' (Fix (M'' a))
data M'' a r = N' | J' a
$(deriveShow1 ''M'') -- template haskell splice

pattern N = M' (Fix N')
pattern J a = M' (Fix (J' a))

-- data Either' a b = Either' a b --------------------------TODO
-- Generator for Either a b.
data Either' a b = Either' (Fix (Either'' a b))
data Either''  a b r= Ri b | Le a
-- $(deriveShow1 'Either") -- template haskell splice
pattern Rii b= Either' (Fix (Ri b))
pattern Lee a = Either' (Fix (Le a))

--  show still needed

-- Generator for ()
type U' = Fix U''
data U'' r = U'''

$(deriveShow1 ''U'')
pattern U = Fix U'''

data L' a = L' (Fix (L'' a)) -- deriving (Show)
data L'' a r = L'' a r | Empti deriving (Show)

$(deriveShow1 ''L'')
pattern L x xs = L' (Fix (L'' x xs))
pattern E = L' (Fix (Empti)) 

data Void' = Void' (Fix Void'')
data Void'' r = Void'' (Void'' r)

--  $(deriveShow1 ''Void'')
--pattern V = Void' V

-- data Identity a = 

--  data Streem' a = Streem' (Fix (Streem'' a))
--  data Streem'' a r = a ::> r

--  $(deriveShow1 ''Streem'')
--  pattern Streem x xs = Streem (Fix (x ::> xs))

-- data Tree' a = Tree' (Fix (Tree'' a )) deriving (Show)
-- data Tree'' a r = Node' a (Forest' r) deriving (Show)
-- data Forest' r = Forest' r | Eimittaan deriving (Show)

--  $(deriveShow1 ''Tree')
--  $(deriveShow1 ''Tree'')
--  $(deriveShow1 ''Forest')

-- pattern Tr xs = Tree' (Fix (xs))
-- pattern Tr' x xs = Node' x (Forest' xs)

-- ... Identity a = --- --------------------------TODO




