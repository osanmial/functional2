{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week2_ex2 where

import Prelude hiding (Functor, fmap)
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Data.Proxy
import Data.Sequence.Internal

class Functor m where
  fmap :: (a -> b) -> m a -> m b


instance Functor (Sum f g) where
  fmap ff (InL x) = undefined
  fmap ff (InR x) = undefined

instance Functor (Product m n) where
  fmap ff (Pair xs ys) = undefined 

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Functor (Compose m n) where
  fmap f (Compose xs) = undefined

instance Functor (Const b) where
  fmap f (Const x) = undefined

-- Proxy can't be meaningfully implemented, because it can't hold data

