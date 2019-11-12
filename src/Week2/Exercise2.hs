{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week2.Exercise2 where

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


--fmap (f.g) (InL x) =? (fmap f . fmap g )  (InL x)
--RHS =(fmap f . fmap g )  (InL x)
-- {From the consitrain  f and g are Functor and by applying  Functor's law}
-- =fmap (f.g) (InL x) 
instance (Functor f, Functor g) => Functor (Sum f g) where
  fmap f (InL x) = InL (fmap f x)                                             -- InL.1
  fmap g (InR x) = InR (fmap g x)

--fmap (f.g) (Pair x y) =? (fmap f . fmap g )  (Pair x y)
-- RHS = (fmap f . fmap g )  (Pair x y)
-- {From the consitrain  f and g are Functor and by applying  Functor's law}
-- = fmap (f.g) (Pair x y)
instance (Functor m, Functor n) => Functor (Product m n) where
  fmap f (Pair x y) = (Pair (fmap f x) (fmap f y))                                        --Pair.1


--fmap (f.g) (Identity x) =? (fmap f.  fmap g) (Identity x)
-- LHS=fmap (f.g) (InL x) 
--{By applying Id.1}
-- =Identity ((f.g)  x)                                                     --Id.3

-- RHS = (fmap f.  fmap g) (Identity x)
-- {From the defintion  of (.)}
-- =fmap f  (fmap g  (Identity x))
-- {By applying Id.1 }
-- =fmap f  (Identity (g x))
-- {By applying Id.1 }
-- =Identity (f  (g  x))
-- {From the defintion  of (.)}
-- =Identity ((f.g)  x)                                                           -- Id.2
--{From Id.2 and Id.3 }
-- RHS==LHS
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)                                      --Id.1 



--fmap (f.g)  (Compose xs)=? (fmap f . fmap g )  (Compose xs)
--RHS =(fmap f . fmap g )  (Compose xs)
-- {From the consitrain  f and g are Functor and by applying  Functor's law}
-- fmap (f.g)  (Compose xs)
instance (Functor m, Functor n) => Functor (Compose m n) where
  fmap f (Compose xs) = (Compose (fmap (fmap f) xs)) where




--fmap (f.g) (Const x) =? (fmap f.  fmap g) (Const x)
-- LHS=fmap (f.g) (Const x) 
--{By applying Const.1}
-- =Const x                                                      --Const.3

-- RHS = (fmap f.  fmap g) (Const x)
-- {From the defintion  of (.)}
-- =fmap f  (fmap g  (Const x))
-- {By applying Const.1 }
-- =fmap f  (Const x)
-- {By applying Const.1 }
-- =Const  x
--{From Const.2 and Const.3 }
-- RHS==LHS
instance Functor (Const a) where
  fmap f (Const x) = Const x                                          --Const.1




--fmap (f.g)  xs =? (fmap f.  fmap g)  xs
-- LHS=fmap (f.g) xs
-- {By applying Proxy.1}
-- = Proxy                                                                    --Proxy.2

-- RHS= (fmap f . fmap g ) xs 
-- {From the defintion  of (.)}
-- = fmap  f (fmap g  xs)
-- {By applying Proxy.1}
-- = famp f  Proxy
-- {By applying Proxy.1}
-- =Proxy                                                                     --Proxy.3
-- {From Proxy.2 and Proxy.3}
-- RHS==LHS
instance Functor Proxy where
  fmap f xs = Proxy                                                       -- Proxy.1

-- 
--fmap (f.g) (State k )=? (fmap f . fmap g) (State k ) 
--LHS=(fmap f . fmap g ) (State k)
-- fmap f (fmap g (State k ))
-- Let f1 (a1,b1)=(a1 , f1 b1) in State (f1.k)
-- =fmap f (fmap g (State xs))
-- = fmap f (State (h.xs))
instance Functor (State s) where
  fmap f (State xs) = State (g . xs)                                 --State.1
    where
      g (a, b) = (a, f b)

-- f :: b -> c
-- e :: (c -> a) -> (b -> a)
-- e ca = ca . f

-- r . e = g
-- r :: (b -> a) -> a
-- g :: (c -> a) -> a
-- ????????????
instance Functor (Cont a) where
  fmap f (Cont xs) = Cont (xs . e)
    where
      e ca = ca . f

-- ??????????
instance Functor m => Functor (Star m a) where
  fmap f (Star xs) = Star(g . xs)
    where
      g = fmap f



--fmap (f.g)  (Costar x)=? (fmap f.  fmap g)  (Costar x)
-- LHS=fmap (f.g)  (Costar x)
--{By applying} Costar.1}
-- =Costar  (f.g.x)                                                               -- Costar.2

--RHS=(fmap f.  fmap g)  (Costar x)
-- {From the defintion  of (.)}
-- =fmap f  (fmap g (Costar x))
--{By applying} Costar.1}
-- =fmap f (Costar g.x)
--{By applying} Costar.1}
-- Costar (f.g.x)                                                                -- Costar.3
--{From Costar.2  and Costar.3}
--RHS==LHS
instance Functor (Costar m a) where
  fmap f (Costar g) = Costar (f . g)                                      --Costar.1




-- ? ? Is that right Yoneda
--fmap (f.g)  (Yoneda x a) =? (fmap f.  fmap g)  (Yoneda x a)
--LHS=fmap (f.g)  (Yoneda x a)
--{By applying Yoneda 1-2}
-- Yoneda (x . a . f . g)

--RHS=(fmap f.  fmap g)  (Yoneda x a)
-- {From the defintion  of (.)}
--famp f (fmap g (Yoneda x a))
-- ??????????????
instance Functor m => Functor (Yoneda m) where
  fmap f (Yoneda xs) = Yoneda (xs . e)                                    --Yoneda.1
    where
      e ca = ca . f                                                                   -- Yoneda.2
-- ??????
instance Functor (Coyoneda m) where
  fmap f (Coyoneda xs ys) = Coyoneda (f . xs) ys
