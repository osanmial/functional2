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

-------------------------------------------------------
-- Generator for Maybe a.
type M' a = Fix (M'' a)
data M'' a r = Nothing | Just a
$(deriveShow1 ''M'') -- template haskell splice

-------------------------------------------------------
-- Generator for Either a b.
type Either' a b = (Fix (Either'' a b))
data Either''  a b r= Ri b | Le a
$(deriveShow1 ''Either'') -- template haskell splice
-------------------------------------------------------
-- Generator for ()
type U' = Fix U''
data U'' r = U'''
$(deriveShow1 ''U'')
pattern U = Fix U'''

type L' a = (Fix (L'' a)) -- deriving (Show)
data L'' a r = L'' a r | Empti deriving (Show)
$(deriveShow1 ''L'')
pattern L x xs = (Fix (L'' x xs))
pattern E =(Fix (Empti)) 
-------------------------------------------------------
type Void' = (Fix Void'')
data Void'' r 
-------------------------------------------------------
type Identity' a = (Fix (Idnetity''))
data Idnetity'' a = Idnetity''  a 
$(deriveShow1 ''Idnetity'')
-------------------------------------------------------
type Streem' a =  (Fix (Streem'' a))
data Streem'' a r = a ::> r
$(deriveShow1 ''Streem'')
pattern Streem x xs = Fix (x ::> xs)
-------------------------------------------------------
type Tree' a = Fix (Tree'' a)
data Tree'' a r = Node' a [r]
--data Forest' r = Forest' r | AllTreesCut deriving (Show)

$(deriveShow1 ''Tree'')
--  $(deriveShow1 ''Forest')

pattern Tr x = Fix x
pattern Tr' x xs = Fix (Node' x (xs))
-- pattern Fr left right = (Forest' left right)
-------------------------------------------------------
type  Expr'   = (Fix (Expr)) 
data Expr r = Add (Expr r)  (Expr r) | Zero | Mul (Expr r)  (Expr r)  | One|
   Let String (Expr r) (Expr r) | Var String 
$(deriveShow1 ''Expr)
-------------------------------------------------------
-- Roll :: f (Free f a) -> Free f a
-- Return :: a -> Free f a
-- data Free f a = Roll (f (Free f a)) | Return a

type Free' f a = Fix (Free'' f a)
data Free'' f a r = Roll (f r) | Return a
$(deriveShow1 ''Free'')

-------------------------------------------------------




-------------------------------------------------------




-------------------------------------------------------
