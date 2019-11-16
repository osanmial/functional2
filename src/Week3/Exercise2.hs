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
import Data.Profunctor
import Data.Sequence.Internal
import Data.Functor.Yoneda


class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  infixl 4 <*>

--------------------------------------------------------------------------------

--there does not appear to be any way to combine InR f and InL a. So no applicative can be found.
-- instance (Applicative f, Applicative g) => Applicative (Sum f g) where
--   pure x = InR $ pure x --- :: Sum f g a -| x :: a
--   (InR f) <*> (InR a) = InR $ f <*> a
--   (InR f) <*> (InL c) = InR $ f <*> c -- no work
--   (InL c) <*> fu  = InL c -- In case we have InL f and InR a then there is no f to convert a to b. as such there cant be a functor instance? 

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = Pair (pure x) (pure x)
  (Pair x1 x2) <*> (Pair y1 y2) = Pair (x1 <*> y1) (x2 <*> y2)

--------------------------------------------------------------------------------

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> functor = fmap f functor

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Very important refrence: enjoy :)
--https://medium.com/@fintan.halpenny/compose-tetris-196b70035aff
  pure x = Compose $ pure $pure x
  Compose f  <*> Compose x=  Compose $ (fmap (<*>)  f ) <*> x
 
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

instance Applicative f => Applicative (Star f d) where
  pure x = Star (\t -> pure x)
  (Star sf) <*> (Star fun) = Star $ \d -> (sf d <*> fun d)

--------------------------------------------------------------------------------

instance Applicative f => Applicative (Costar f c) where
  pure x = Costar (\fd -> x)
  (Costar csf) <*> (Costar fun) = Costar (\fd -> (csf fd) (fun fd))
  
--------------------------------------------------------------------------------  
-- idk check hoogle source
instance Applicative m => Applicative (Yoneda m) where
  pure x = Yoneda $ \y -> pure (y x)
  Yoneda f <*> Yoneda x = Yoneda $ (\y -> f (y .) <*> x id)



  
