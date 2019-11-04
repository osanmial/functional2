module Week2_ex1 where

import Prelude hiding (Functor, fmap)
import Data.List.NonEmpty
import Data.Monoid.Endo

class Functor m where
  fmap :: (a -> b) -> m a -> m b


-- Bool doesn't admit a functor, because it's not a wrapper of any kind

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing

instance Functor (Either a) where
  fmap f (Right y) = Right (f y)
  fmap _ (Left x)  = Left x

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

-- Endo a contains function of type a -> a and in fmap we have no function to convert the input value of the function into the end type endo b where the function would be of type b -> b as we only have a function of type (a -> b) in fmap.
-- instance Functor Endo where
--   fmap f (Endo g) = Endo ()

instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs
  
