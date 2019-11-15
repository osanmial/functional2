{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week3.Excercise2 where

import Prelude hiding (Applicative, pure, (<*>))
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Control.Dsl.Cont as C
import Data.Proxy
import Data.Sequence.Internal

--------------------------------------------------------------------------------

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  infixl 4 <*>

--------------------------------------------------------------------------------

{-
identity
    pure id <*> v = v

composition
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

homomorphism
    pure f <*> pure x = pure (f x)

interchange
    u <*> pure y = pure ($ y) <*> u
-}
    
-- here are multiple different ways to define this. 
instance (Applicative f, Applicative g) => Applicative (Sum f g) where
  pure x = InR pure x --- :: Sum f g a -| x :: a
  (InR f) <*> fu = f <$> fu
  (InL f) <*> fu  = inL f

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = undefined
  (Pair x1 x2) <*> (Pair y1 y2) = Pair (x1 <*> y1) (x2 <*> y2)

--------------------------------------------------------------------------------

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> functor = fmap f functor

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = undefined
  (Compose f) <*> functor = undefined-- fmap f functor
                  -- Expected type (a -> b), ^ Actual type f (g (a -> b)) 

--------------------------------------------------------------------------------

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const f <*> Const v = Const (f <> v)

--------------------------------------------------------------------------------

instance Applicative Proxy where
  pure x = Proxy
  x <*> y = Proxy

--------------------------------------------------------------------------------

instance Applicative (State s) where
  pure x = State (\s -> (s, x))
  State f <*> State g=  State $ \ h ->(h, ((snd  (f h)) (snd  (g h))))
   
--------------------------------------------------------------------------------

instance Applicative (Cont a) where
    pure x = Cont (\f-> f x) 
    Cont  f <*> Cont g = Cont $ \ h -> f $ \ k -> g $ \ x -> h (k x)
    
instance Functor (Cont a) where
  fmap f (Cont xs) = Cont (xs . e)
    where
      e ca = ca . f

--------------------------------------------------------------------------------

instance Applicative (Star f d) where
  pure x = Star (\t -> pure x)
  
  
  
