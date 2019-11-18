module Week4.Exercise1 where
import Prelude hiding (Monad, return, (>>=))
import Data.List.NonEmpty as Esko


class Applicative m => Monad m where  
    return :: a -> m a
    return = pure
    (>>=) :: m a -> (a -> m b) -> m b
    (>>)  :: m a -> m b -> m b
    m >> n = m >>= \_ -> n
--------------------------------------------------------------------------------

-- Bool is not a Monad

--------------------------------------------------------------------------------
instance Monad Maybe where
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
--------------------------------------------------------------------------------
instance Monad [] where
    xs >>= f = concat (Prelude.map f xs)  
--------------------------------------------------------------------------------
instance Monad NonEmpty where
  (x :| xs) >>= f = y :| ys ++ zs
    where
      (y :| ys) = f x
      zs = xs >>= (toList . f)
      toList (c :| cs) = c:cs
--------------------------------------------------------------------------------
instance Monoid a => Monad ((,) a) where
    (x,y) >>= f = (x, snd (f y))
--------------------------------------------------------------------------------

