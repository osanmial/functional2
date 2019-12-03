module Week6.Exercise1 where

import Data.Functor.Compose -- Why imported?
import Data.Functor.Const
import Data.Functor.Identity

newtype WrappedTraversable m a = WrapTraversable {unwrapTraversable :: m a}
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
instance Traversable m => Functor (WrappedTraversable m) where
  -- runIdentity :: Identity a -> a
  -- identity :: a -> identity a
  -- f :: a -> b 
  -- Identity:: a -> identity b
  --fmap f (WrapTraversable xs) =  WrapTraversable $  runIdentity.traverse (Identity . f) <$> xs
  fmap f =  runIdentity.traverse (Identity . f) 
  -- this also works but why the above doesn't need to unwrap? Because newtype I guess?
  -- fmap f (WrapTraversable xs) =  WrapTraversable . runIdentity . traverse (Identity . f) $ xs

instance Traversable m => Foldable (WrappedTraversable m) where
  -- Const :: a -> Const a b
  -- getConst :: Const a b -> a
  -- f :: a -> m 
  foldMap f  = getConst . traverse (Const . f)

instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA = traverse id
  traverse k = traverse k
--  traverse k (WrapTraversable xs) =  WrapTraversable <$> traverse k xs
