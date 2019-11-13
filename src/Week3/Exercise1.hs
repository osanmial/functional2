{-# LANGUAGE DeriveAnyClass #-}

module Week3.Exercise1 where

import Prelude hiding (Applicative, pure, (<*>))
import Data.List.NonEmpty
import Data.Map as Map
import Data.Foldable as F



class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  infixl 4 <*>


--------------------------------------------------------------------------------

-- Bool is not a functor, so it can't be a applicative

--------------------------------------------------------------------------------

instance Applicative Maybe where
  pure x = Just x
  Nothing <*> _       = Nothing
  Just f  <*> functor = fmap f functor

--------------------------------------------------------------------------------

instance Applicative (Either a) where
  pure x = Right x
  Right f <*> functor = fmap f functor
  Left x  <*> functor = Left x

--------------------------------------------------------------------------------

instance Monoid a => Applicative ((,) a) where
  pure x = (mempty, x)
  (x, f) <*> (y, z) = (x <> y, f z)

--------------------------------------------------------------------------------

-- ENDO

--------------------------------------------------------------------------------

instance Applicative ((->) a) where
  pure x = const x
  f <*> functor = \x -> f x (functor x)
  
--------------------------------------------------------------------------------

-- () is not a functor

--------------------------------------------------------------------------------

instance Applicative [] where
  pure x = [x]
  []     <*> _ = []
  (f:fs) <*> functor = (fmap f functor) ++ (fs <*> functor)

--------------------------------------------------------------------------------

instance Applicative NonEmpty where
  pure x = (x :| [])
  (f :| fs) <*> (v :| vs) = (f v) :| ( (f <$> vs) <> (fs <*>(v:vs)))

--------------------------------------------------------------------------------

-- Void is not a functor

--------------------------------------------------------------------------------

instance Applicative IO where
  pure x = return x
  mf <*> functor = do
    f <- mf
    fmap f functor
    
--------------------------------------------------------------------------------

-- identity
--     pure id <*> v = v
--     Combination of mempty in the keys of map does not lead in to new elements
--     as such the id just gets applied to every value contained in v and that should lead to the same v
--
-- composition
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--     If we convert the input map essentially into an array we can make an instance if our key value is iterable and has identity element. Then this is essentially the same as for a list.
--     Wont work if the keys are plain monoids as maps with keys [(sum 1)] [sum 1,sum 2], [sum 1,sum 2] could be combined depending on the order of applications to [Sum 3, Sum4, Sum 5, Sum 6] or [Sum 3, Sum 4, Sum 5]
--Seems like it might work with numbers also
--examples of how we can lose important  data.
--types
-- u :: f (a -> b)
-- v :: f (c -> a)
-- w :: f w
--     pure (.) <*> u produces us a map of functions.
--     in case of same map keys the result will always be the combination that results from the last value on the first map. No keys will be lost as the result will only be when there already was multiple candidates. 
--     
-- homomorphism
--     pure f <*> pure x = pure (f x)
--     as all of them have the same one mempty key the result will hold as nothing interesting happens with the structure.
--
-- interchange
--     u <*> pure y = pure ($ y) <*> u
--     when there is only mempty key on one side. Should work just as for lists

--  ∀ a b  : a <> b = c , a != c, b!=c

class (Ord a) => OrdIterable a where
  first :: a
  next :: a -> a
  toKeys :: [b] -> [(a,b)]
  --toKeys' :: Foldable t => t a -> t k

instance OrdIterable Sum where
  first = Sum 0
  next a = a <> Sum 1
  toKeys list = toKeys' (mempty) list where
    toKeys' n (x:xs) = (n,x):toKeys' (n <> Sum 1) xs

--data (Monoid k) => MapM k a = Map k a

--This essentially converts the problem into a list and as such works
--instance (OrdIterable k) => Applicative (Map k) where
--  pure x = singleton first x
--  mapf <*> map2 = Map.fromList (toKeys $ (F.toList mapf) <*> (F.toList map2))

    
instance (Monoid k, Ord k) => Applicative (Map k) where
  pure x = singleton mempty x
  mapf <*> map2 = Map.fromList (f <$> (Map.toList mapf) <*> (Map.toList map2)) where
    f :: Monoid a => (a, t -> b) -> (a, t) -> (a, b)
    f (a,g) (c,d) =(a <> c, g d)

--For example usage
data Sum = Sum {inttiUlos :: Int} deriving (Eq, Ord, Show)
instance Semigroup Sum where
  (Sum a) <> (Sum b) = Sum $ a + b
instance Monoid Sum where
  mempty = (Sum 0)
both f (a,b) = (f a,f b)

--Example usage
test =
  (both inttiUlos) <$>
  (Map.toList $
    (Map.fromList $ (\(a,b) -> (Sum a,(Sum b <>))) <$> [(1,1),(1,10)]) <*>
    (Map.fromList $ (both Sum) <$> [(1,1),(2,2),(3,3),(4,4)]))




