module Week6.Exercise1 where

import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity

newtype WrappedTraversable m a = WrapTraversable {unwrapTraversable :: m a}

instance Traversable m => Functor (WrappedTraversable m) where
  fmap f (WrapTraversable xs) = undefined

instance Traversable m => Foldable (WrappedTraversable m) where
  foldMap f (WrapTraversable xs) = undefined

instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA (WrapTraversable xs) = undefined
  traverse k (WrapTraversable xs) = undefined