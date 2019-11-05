module Week2_ex1 where

import Prelude hiding (Functor, fmap, map)
import Data.Monoid.Endo -- endo
import Data.Functor.Contravariant hiding (Contravariant) -- base
import Data.List.NonEmpty hiding (map) -- containers?
import Data.Map as Map


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
--for contravariant the same applies, but backwards.
--For multi kind functors the problem is that endo has only one kind. 



instance Functor ((->) x) where
   fmap f g = f . g

--Contravarian wont work as we have no access to the input value of the function.
--instance Contravariant ((->) x) where
--   contramap f g = f . g

-- wont work as we have no function to handle the input value
--instance Bifunctor (->) where
--  bimap f g h = 

instance Profunctor (->) where
  dimap f g h = g . h . f
  
instance Contravariant (Op x) where
  contramap f (Op g) = Op (g . f)
-- f :: (a -> b)
-- g :: (b -> x)
-- ~>  (a -> x)

-- Bifunctor works not because it does not have a function for the intput value of Op.

--does not work because f g ar backwards in comparison to Op
--instance Profunctor (Op) where
--  dimap f g (Op h) = (f . h . g)
-- f :: (a -> b)
-- g :: (c -> d)
-- h :: m b c
-- out m a b
    


--instance Bifunctor

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

