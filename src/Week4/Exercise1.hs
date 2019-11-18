{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week4.Exercise1 where
import Prelude hiding (Monad, return, (>>=), (>>))
import Data.List.NonEmpty as Esko hiding (map)

import GHC.Base hiding (Monad, (>>=), (>>))
import GHC.ST
import GHC.Exception
import GHC.Show
import Data.Maybe

import {-# SOURCE #-} GHC.IO.Exception ( userError )


class Applicative m => Monad m where  
    return :: a -> m a
    return = pure
    (>>=) :: m a -> (a -> m b) -> m b
    (>>)  :: m a -> m b -> m b
    m >> n = m >>= \_ -> n

--------------------------------------------------------------------------------

-- Bool is not a Monad

---------------------------------------------------------------------------------

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f  = f x
    
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

instance Monad (Either a ) where
   return x= Right x 
   (Right x ) >>= f = f x
   (Left x ) >>= f = Left x
   
---------------------------------------------------------------------------------

instance Monad [] where
    xs >>= f = concat (Prelude.map f xs)
    
---------------------------------------------------------------------------------

instance Monad NonEmpty where
  (x :| xs) >>= f = y :| ys ++ zs
    where
      (y :| ys) = f x
      zs = xs >>= (toList . f)
      toList (c :| cs) = c:cs
      
---------------------------------------------------------------------------------

instance Monoid a => Monad ((,) a) where
    (x,y) >>= f = (x, snd (f y))
    
---------------------------------------------------------------------------------

instance Monad IO where
  (IO m) >>= am = IO $ \s -> case m s of
    (# s', r #) -> case (am r) of 
      (IO am') -> am' s'

---------------------------------------------------------------------------------

-- Map if we have time
