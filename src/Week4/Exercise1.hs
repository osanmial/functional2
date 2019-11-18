module Week4.Exercise1 where
import Prelude hiding (Monad, return, (>>=))


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
    xs >>= f = concat (map f xs)  
 --------------------------------------------------------------------------------
 
instance Monoid a => Monad ((,) a) where
    return x = (mempty,x)
    (x,y) >>= f = (x, snd (f y))
--------------------------------------------------------------------------------
