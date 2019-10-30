{-# Language FlexibleInstances#-}
module Week1_2 where

import Prelude hiding (Semigroup, Monoid, (<>), mempty)
import Data.Either
import Data.List.NonEmpty
import Data.Map as Map

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a


-- AND operator is accociative
-- a && (b && c) = (a && b) && c 
instance Semigroup Bool where
  a <> b = a && b

-- True is mempty in terms of AND operation
instance Monoid Bool where
  mempty = True



instance Semigroup a => Semigroup (Maybe a) where
  (Just xs) <> (Just ys) = Just (xs <> ys)
  Nothing  <> ys         = ys
  xs       <> Nothing    = xs

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing


-- Show doesn't work on this for some reason
instance (Semigroup a, Semigroup b) => Semigroup (Either a b) where
  (Left x)  <> (Left y)  = Left  (x <> y)
  (Right x) <> (Right y) = Right (x <> y)
  (Left x)  <> _         = Left x
  _         <> Left y    = Left y

instance (Monoid a, Monoid b) => Monoid (Either a b) where
  mempty = Right mempty


instance Semigroup (a -> a) where
  _ <> _ = id 

instance Monoid (a -> a) where
  mempty = id
  

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
 (x1, y1) <> (x2, y2) = ((x1 <> x2), (y1 <> y2))

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)


instance Semigroup () where
  _ <> _ = ()

instance Monoid () where
  mempty = ()


instance Semigroup [a] where
  xs <> ys = xs ++ ys

instance Monoid [a] where
  mempty = []


instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ y:ys)


instance (Ord k, Semigroup a) => Semigroup (Map k a) where
  xs <> ys = unionWith (<>) xs ys

instance (Ord k, Monoid a) => Monoid (Map k a) where
  mempty = empty
