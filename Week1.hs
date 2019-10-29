{-# Language FlexibleInstances #-}
module Week1where

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

-- 
instance Eq a => Eq (Maybe a) where
  (==) (Just x) (Just y) = x == y
  (==) Nothing Nothing   = True
  (==) _       _         = False

instance (Eq a, Eq b) => Eq (Either a b) where
  (==) (Left x) (Left y)   = x == y
  (==) (Right x) (Right y) = x == y
  (==) _         _         = False

instance (Eq a, Eq b) => Eq (a, b) where
  (==) (x1, x2) (y1, y2) = (x1 == y1) && (x2 == y2)

instance Eq a => Eq (a -> a) where
  (==) _ _ = True

instance Eq a => Eq [a] where
  --(==) []  []       = True
  (==) (x:xs) (y:ys) = (x == y) && (xs == ys)
  (==) _      _      = False

instance Eq () where
  (==) x y = True

instance Eq a => Eq (NonEmpty a) where
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

instance Eq Void where
  (==) a b = True


instance (Ord k, Eq a) => Eq (Map k a) where
  (==) xs ys = isSubmapOfBy (==) xs ys && isSubmapOfBy (==) ys xs  


fun :: [a] -> [a] -> Bool
fun []      ys    = False
fun []      []    = True
fun [x] (y:ys)    = True 
