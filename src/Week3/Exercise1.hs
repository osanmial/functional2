{-# LANGUAGE DeriveAnyClass, InstanceSigs #-}

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

-- Op not a functor

--------------------------------------------------------------------------------

instance Applicative ((->) a) where
  pure x = const x
  f <*> functor = \x -> f x (functor x)
  
--------------------------------------------------------------------------------

-- () is not a functor

--------------------------------------------------------------------------------

instance Applicative [] where
  pure x = [x]
  (<*>) :: [a -> b] -> [a] -> [b]
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

toKeys :: Enum b => b -> [a] -> [(b,a)]
tokeys k [] = []
toKeys k (x:xs) = (k,x):(toKeys (succ k) xs)


--This essentially converts the map into a list and as such works.
instance (Enum k, Ord k) => Applicative (Map k) where
  pure x = singleton (toEnum 0) x
  mapf <*> map2 = Map.fromList (toKeys (toEnum 0) $ (F.toList mapf) <*> (F.toList map2))

{-
--Not a real applicative
instance (Monoid k, Ord k) => Applicative (Map k) where
  pure x = singleton mempty x
  mapf <*> map2 = Map.fromList (f <$> (Map.toAscList mapf) <*> (Map.toAscList map2)) where
    f :: Monoid a => (a, t -> b) -> (a, t) -> (a, b)
    f (a,g) (c,d) =(a <> c, g d)
    --voidaan koravta (<*>)
-}

--For example usage
data Sum = Sum {inttiUlos :: Int} deriving (Eq, Ord, Show)
instance Semigroup Sum where
  (Sum a) <> (Sum b) = Sum $ a + b
instance Monoid Sum where
  mempty = (Sum 0)
instance Enum Sum where
  fromEnum (Sum x) = x
  toEnum x = Sum x
both f (a,b) = (f a,f b)

--Example usage
test =
  (both inttiUlos) <$>
  (Map.toList $
    (Map.fromList $ (\(a,b) -> (Sum a,(Sum b <>))) <$> [(1,1),(1,10)]) <*>
    (Map.fromList $ (both Sum) <$> [(1,1),(2,2),(3,3),(4,4)]))




