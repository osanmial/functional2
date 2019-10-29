{-# Language FlexibleInstances #-}
module Week1 where

import Prelude hiding (Eq, (==))
import Data.List.NonEmpty 
import Data.Void
import Data.Map as Map

class Eq a where
  (==) :: a -> a -> Bool

-- Bools are a finite set of predetermined values,
-- so you can check states against each other
instance Eq Bool where
  (==) True True   = True
  (==) False False = True
  (==) _     _     = False

-- Same deal as bool, addition of 'a' doesn't change things significantly
instance Eq a => Eq (Maybe a) where
  (==) (Just x) (Just y) = x == y
  (==) Nothing Nothing   = True
  (==) _       _         = False

-- Similar situation as the previous two
-- but Lefts and Rights can't be tested reliably, because 'a' and 'b' might be different types
-- So 'Left 1 == Right 1' is still False
instance (Eq a, Eq b) => Eq (Either a b) where
  (==) (Left x) (Left y)   = x == y
  (==) (Right x) (Right y) = x == y
  (==) _         _         = False

-- If values inside tuples match, the tuples themselves match
instance (Eq a, Eq b) => Eq (a, b) where
  (==) (x1, x2) (y1, y2) = (x1 == y1) && (x2 == y2)

-- (a -> a) is an identity function, so this is always True
instance Eq a => Eq (a -> a) where
  (==) _ _ = True

-- Same as with tuples, if all values inside two lists match, they are similar
-- Empty list cannot be handled for some reason
instance Eq a => Eq [a] where
  --(==) []  []       = True
  (==) (x:xs) (y:ys) = (x == y) && (xs == ys)
  (==) _      _      = False

-- () has only one instance, so it's always True
instance Eq () where
  (==) x y = True

-- Similar to list, but you don't have worry about empty lists.
instance Eq a => Eq (NonEmpty a) where
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

-- Void has no instances so this can be defined, but it can never be called
instance Eq Void where
  (==) a b = True

instance (Ord k, Eq a) => Eq (Map k a) where
  (==) xs ys = isSubmapOfBy (==) xs ys && isSubmapOfBy (==) ys xs  

