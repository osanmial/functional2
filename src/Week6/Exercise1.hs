module Week6.Exercise1 where

import Data.Functor.Compose -- Why imported?
import Data.Functor.Const
import Data.Functor.Identity

newtype WrappedTraversable m a = WrapTraversable {unwrapTraversable :: m a}

instance Traversable m => Functor (WrappedTraversable m) where
  fmap f =  runIdentity . traverse (Identity . f)
  -- this also works but why the above doesn't need to unwrap? Because newtype I guess?
  -- fmap f (WrapTraversable xs) =  WrapTraversable . runIdentity . traverse (Identity . f) $ xs

instance Traversable m => Foldable (WrappedTraversable m) where
  foldMap f  = getConst . traverse (Const . f)

instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA = traverse id
  traverse k = traverse k
--  traverse k (WrapTraversable xs) =  WrapTraversable <$> traverse k xs
