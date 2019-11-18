{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week4.Exercise1 where
import Prelude hiding (Monad, return, (>>=))
--import Week3.Exercise1

class Applicative  m=> Monad m where  
    return :: a -> m a  
    (>>=) :: m a -> (a -> m b) -> m b  
---------------------------------------------------------------------------------

-- Bool is not a Monad

---------------------------------------------------------------------------------
instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    
---------------------------------------------------------------------------------

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)

---------------------------------------------------------------------------------

---nstance Monad ((,)a) where
 --  return x = (empty,x)
 --  (x,y) >>= f = undefined

---------------------------------------------------------------------------------

-- Endo not applicative so not monad.

---------------------------------------------------------------------------------

-- Op not a functor so not an applicative so not a monad

---------------------------------------------------------------------------------

instance Monad ((->) c) where
  return = pure
  (>>=) :: (a1 -> a) -> (a -> a1 -> b) -> a1 -> b
  ar >>= f = (\a1 -> f (ar a1) a1 ) -- f . ar
    -- (\a -> f (ar a))

---------------------------------------------------------------------------------

-- No instance for Void as it is of the wrong type kind

---------------------------------------------------------------------------------

{-
defined something like:
data IO = (\s -> (s,a))
where
  getMaskingState# s
gives us t

-}


instance Monad (IO a1) where
  a >>= b = 
    a = 
