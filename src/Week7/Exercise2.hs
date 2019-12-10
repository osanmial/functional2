{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFoldable, GADTs, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
module Week7.Exercise2 where

import Data.Foldable.Deriving
--import Data.Foldable.Deriving (deriveFoldable)
--import Data.Functor.Classes
--import Data.Functor.Deriving (deriveFunctor)
--import Data.Traversable.Deriving (deriveTraversable)
-- import Text.Show.Deriving (deriveShow1)

newtype Fix m = Fix {unFix :: m (Fix m)}


--Mitä helvetin pattern synonyymejä?
type Bool' = Fix (Const Bool'') 
data Bool'' = T | F

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



