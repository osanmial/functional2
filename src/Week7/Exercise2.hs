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
-------------------------------------------------------
type L' a = (Fix (L'' a)) -- deriving (Show)
data L'' a r = L'' a r | Empti deriving (Show)
$(deriveShow1 ''L'')
-------------------------------------------------------
type Void' =  (Fix Void'')
data Void'' r = Void'' (Void'' r)
$(deriveShow1 ''Void'')
-------------------------------------------------------
type Identity' a = (Fix (Identity''))
data Identity'' a= Identity''  a 
$(deriveShow1 ''Identity'')
-------------------------------------------------------
type Streem' a =  (Fix (Streem'' a))
data Streem'' a r = a ::> r
$(deriveShow1 ''Streem'')
-------------------------------------------------------

--type Tree' a = Fix (Tree'' a ) 
--data Tree'' a r = Node' a (Forest') 
-- $(deriveShow1 ''Tree')
--
--type Forest'= Fix Forest''
--data Forest'' r = Forest' r | Eimittaan 
-- --$(deriveShow1 ''Forest')

-- pattern Tr xs = Tree' (Fix (xs))
-- pattern Tr' x xs = Node' x (Forest' xs)
-------------------------------------------------------
type  Expr'   = (Fix (Expr)) 
data Expr a = Add (Expr a)  (Expr a) | Zero | Mul (Expr a)  (Expr a)  | One|
   Let String (Expr a) (Expr a) | Var String 
$(deriveShow1 ''Expr)
-------------------------------------------------------
-- Roll :: f (Free f a) -> Free f a
-- Return :: a -> Free f a
data Free f a = Roll (f (Free f a)) | Return a
-------------------------------------------------------





-------------------------------------------------------




-------------------------------------------------------
