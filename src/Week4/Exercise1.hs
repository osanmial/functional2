module Week4.Exercise1 where
import Prelude hiding (Monad, return, (>>=)) 
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map

class Applicative  m=> Monad m where  
    return :: a -> m a  
    (>>=) :: m a -> (a -> m b) -> m b  
--------------------------------------------------------------------------------

-- Bool is not a Monad

--------------------------------------------------------------------------------
instance Monad Maybe where
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
 --------------------------------------------------------------------------------
instance Monad [] where  
    return x = [x]  
    xs >>= f = Prelude.concat (Prelude.map f xs)  
 --------------------------------------------------------------------------------
 
instance Monoid a => Monad ((,) a) where
    return x = (mempty,x)
    (x,y) >>= f = (x, snd (f y))
--------------------------------------------------------------------------------
instance Monad (Either a ) where
   return x= Right x 
   (Right x ) >>= f = f x
   (Left x ) >>= f = Left x 
--------------------------------------------------------------------------------
instance Monad NonEmpty where
   return x = (x :| [])
   (x :| xs) >>= f =  f x :| ( xs >>= f) 
--------------------------------------------------------------------------------
--instance for Map will be implemented later  if we have more time
--------------------------------------------------------------------------------    
   
   
   
   