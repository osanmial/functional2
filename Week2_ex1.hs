module Week2_ex1 where

import Prelude hiding (Functor, fmap)
import Data.List.NonEmpty

class Functor m where
  fmap :: (a -> b) -> m a -> m b


instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing

instance Functor (Either a) where
  fmap f (Right y) = Right (f y)
  fmap _ (Left x)  = Left x

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs
  
