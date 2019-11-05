{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week2_ex2 where

import Prelude hiding (Functor, fmap)
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Const
import Data.Proxy
import Data.Sequence.Internal
import Control.Dsl.Cont
import Data.Profunctor
import Data.Functor.Yoneda
import Data.Functor.Coyoneda


class Functor m where
  fmap :: (a -> b) -> m a -> m b


instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap ff (InL x) = InL (fmap ff x)
  fmap ff (InR x) = InR (fmap ff x)

instance (Functor m, Functor n) => Functor (Product m n) where
  fmap ff (Pair xs ys) = (Pair (fmap ff xs) (fmap ff ys))

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance (Functor m, Functor n) => Functor (Compose m n) where
  fmap f (Compose xs) = (Compose (fmap f' xs)) where
    f' x = fmap f x  

instance Functor (Const a) where
  fmap f (Const x) = Const x

instance Functor Proxy where
  fmap f xs = Proxy

instance Functor (State s) where
  fmap f (State xs) = State (g . xs)
    where
      g (a, b) = (a, f b)

-- f :: b -> c
-- e :: (c -> a) -> (b -> a)
-- e ca = ca . f

-- r . e = g
-- r :: (b -> a) -> a
-- g :: (c -> a) -> a
instance Functor (Cont a) where
  fmap f (Cont xs) = Cont (xs . e)
    where
      e ca = ca . f

instance Functor m => Functor (Star m a) where
  fmap f (Star xs) = Star(g . xs)
    where
      g = fmap f

instance Functor (Costar m a) where
  fmap f (Costar g) = Costar (f . g)

instance Functor m => Functor (Yoneda m) where
  fmap f (Yoneda xs) = undefined

instance Functor (Coyoneda m) where
  fmap f (Coyoneda _ _) = undefined
