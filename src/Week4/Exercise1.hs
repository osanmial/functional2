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

 -------------------------------------------------------------------------------

instance Monad (Either a ) where
   return x= Right x 
   (Right x ) >>= f = f x
   (Left x ) >>= f = Left x
   
--------------------------------------------------------------------------------
instance Monoid a => Monad ((,) a) where
    return x = (mempty,x)
    (x,y) >>= f = (x, snd (f y))

---------------------------------------------------------------------------------

-- Endo not applicative so not monad.

---------------------------------------------------------------------------------

instance Monad ((->) c) where
  return = pure
  (>>=) :: (a1 -> a) -> (a -> a1 -> b) -> a1 -> b
  ar >>= f = (\a1 -> f (ar a1) a1 ) -- f . ar
    -- (\a -> f (ar a))

---------------------------------------------------------------------------------

-- Op not a functor so not an applicative so not a monad

---------------------------------------------------------------------------------

-- () is not a functor

--------------------------------------------------------------------------------    
instance Monad [] where  
    return x = [x]  
    xs >>= f = Prelude.concat (Prelude.map f xs)  
 
--------------------------------------------------------------------------------    

instance Monad NonEmpty where
  (x :| xs) >>= f = y :| ys ++ zs
    where
      (y :| ys) = f x
      zs = xs >>= (toList . f)
      toList (c :| cs) = c:cs

--------------------------------------------------------------------------------

-- No instance for Void as it is of the wrong type kind

---------------------------------------------------------------------------------

instance Monad IO where
  (IO m) >>= am = IO $ \s -> case m s of
    (# s', r #) -> case (am r) of 
      (IO am') -> am' s'

---------------------------------------------------------------------------------

--instance for Map will be implemented later  if we have more time
-- Map if we have time

