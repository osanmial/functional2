module Week2_ex1 where

import Prelude hiding (Functor, fmap, map)
import Data.List.NonEmpty hiding (map)
import Data.Map as Map
import Data.Monoid.Endo

class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Contravariant m where
  contramap :: (a -> b) -> m b -> m a

class Bifunctor m where
  bimap :: (a -> b) -> (c -> d) -> m a c -> m b d

class Profunctor m where
  dimap :: (a -> b) -> (c -> d) -> m b c -> m a d

-- Bool doesn't admit a functor, because it's not a wrapper of any kind

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing

instance Functor (Either a) where
  fmap f (Right y) = Right (f y)
  fmap _ (Left x)  = Left x

instance Bifunctor Either where
  bimap f g (Left x)  = Left (f x)
  bimap f g (Right y) = Right (g y)
  
instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)

instance Bifunctor (,) where
  bimap f g (x, y) = (f x, g y)

-- Endo a contains function of type a -> a and in fmap we have no function to convert the input value of the function into the end type endo b where the function would be of type b -> b as we only have a function of type (a -> b) in fmap.
-- instance Functor Endo where
--   fmap f (Endo g) = Endo ()

--() is of kind :: * so it can't be mapped over

instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

-- Void is of wrong kind

instance Functor IO where
  fmap f i = do
    x <- i
    let b = f x
    pure b

instance Functor (Map k) where
  fmap f xs = map f xs
