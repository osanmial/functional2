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
  traverse k ma = sequenceA $ fmap k ma
  -- I will require some form of traversable function to already be implmented in order to do either of the above as
  -- as the unique properties custom to traversable are required.

  -- m (f b)
  -- > f (m b)

--  traverse k (WrapTraversable xs) =  WrapTraversable <$> traverse k xs
-- traverse :: (a -> f b) -> W m a -> f (W m b)
--     fmap :: (a -> b)   -> W m a -> W m b
--     fmap :: (a -> f b) -> W m a -> W m (f b)

--  foldMap :: (a -> m1) -> W m a -> m1


-- (f b) -> b

--     fmap :: (a -> f b)   -> m a -> m (f b)
