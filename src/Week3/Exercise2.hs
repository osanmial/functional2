{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
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
import Data.Functor.Yoneda

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  infixl 4 <*>

--------------------------------------------------------------------------------

-- Pure for Sum f g a is probably impossible
-- we don't have an instance of g or f to make pure. So this is not an applicative
{-    
instance (Applicative f, Applicative g) => Applicative (Sum f g) where
  pure x = undefined --- :: Sum f g a -| x :: a
  (InL f) <*> (InL y) = InL (f <*> y)
  (InR f) <*> (InR y) = InR (f <*> y)
-}
--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure x = Pair (pure x) (pure x)
  (Pair x1 x2) <*> (Pair y1 y2) = Pair (x1 <*> y1) (x2 <*> y2)

--------------------------------------------------------------------------------

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> functor = fmap f functor

--------------------------------------------------------------------------------

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const f <*> Const v = Const (f <> v)

--------------------------------------------------------------------------------

instance Applicative (Cont a) where
    pure x = Cont (\f-> f x) 
    Cont  f <*> Cont g = Cont $ \ h -> f $ \ k -> g $ \ x -> h (k x)
    
instance Functor (Cont a) where
  fmap f (Cont xs) = Cont (xs . e)
    where
      e ca = ca . f

--------------------------------------------------------------------------------

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose (pure (pure x))
  (Compose f) <*> (Compose x) = undefined

--------------------------------------------------------------------------------

instance Applicative Proxy where
  pure x = Proxy
  x <*> y = Proxy

--------------------------------------------------------------------------------

instance Applicative (State s) where
  pure x = State (\s -> (s, x))
  State f <*> State g=  State $ \ h ->(h, ((snd  (f h)) (snd  (g h))))
   
--------------------------------------------------------------------------------

-- idk check hoogle source
instance Applicative m => Applicative (Yoneda m) where
  pure x = Yoneda $ \y -> pure (y x)
  Yoneda f <*> Yoneda x = Yoneda $ (\y -> f (y .) <*> x id)

--------------------------------------------------------------------------------
  
