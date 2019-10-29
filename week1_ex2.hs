module Week1_2 where

import Prelude hiding (Semigroup, Monoid, (<>), mempty)

class Semigroup a where
  (<>) :: a -> a -> a

class Monoid a where
  mempty :: a


instance Semigroup Bool where
  a <> b = a && b

instance Monoid Bool where
  mempty = True


instance Semigroup a => Semigroup (Maybe a) where
  (Just xs) <> (Just ys) = Just (xs <> ys)
  Nothing  <> ys         = ys
  xs       <> Nothing    = xs

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing


-- ???
instance (Semigroup a, Semigroup b) => Semigroup (Either a b) where
  (Left x)  <> (Left y)  = Left  (x <> y)
  (Right x) <> (Right y) = Right (x <> y)


instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
 (x1, y1) <> (x2, y2) = ((x1 <> x2), (y1 <> y2))

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)


instance Semigroup [a] where
  xs <> ys = xs ++ ys

instance Monoid [a] where
  mempty = []
