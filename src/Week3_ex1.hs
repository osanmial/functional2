module Week3_ex1 where

import Prelude hiding (Applicative, pure, (<*>))
import Data.List.NonEmpty
import Data.Map as Map

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

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
  f <*> functor = \c -> (f c) (functor c)
-- I have found this applicative in the refrence book 
--instance Applicative ((->) r) where  
--    pure x = (\_ -> x)  
--    f <*> g = \x -> f x (g x)  
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

instance (Monoid k,Ord k) => Applicative (Map k) where
  pure x = singleton mempty x
  mapf <*> map2 = Map.fromList $ (\(a,g) (c,d) -> (a <> c, g d)) <$> (Map.toList mapf) <*> (Map.toList map2)

