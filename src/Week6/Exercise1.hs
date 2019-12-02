module Week6.Exercise1 where

import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity

newtype WrappedTraversable m a = WrapTraversable {unwrapTraversable :: m a}

instance Traversable m => Functor (WrappedTraversable m) where
  fmap f (WrapTraversable xs) = WrapTraversable (traverse g xs) where
    g =  . f 

-- f :: a -> b
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)

  

instance Traversable m => Foldable (WrappedTraversable m) where
  foldMap f (WrapTraversable xs) = foldMap f xs

instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA = traverse id
  traverse k (WrapTraversable xs) =  WrapTraversable <$> traverse k xs




