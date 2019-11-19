module Week4.Exercise2 where

import Prelude hiding (Monad, return, (>>=), (>>))
import Week4.Exercise1
import Utility.Complex  


instance (Monad m , Monad n)=>Monad (Product m n)  where
    return x =Pair (pure x) (pure x)
    (Pair x1 y1) >>= f=  Pair (x1 >>= (fst'.f)) (y1 >>= (snd'.f))
            where 
                fst' (Pair fx _)= fx
                snd' (Pair _ fy)= fy
--------------------------------------------------------------------------------

-- Sum m n a is not applicative so no instance

--------------------------------------------------------------------------------
instance Monad Identity where
    return x = Identity x1
    (Identity x) >>= f = f x 

--------------------------------------------------------------------------------

-- TODO Compose m n a, instances for m and n given

--------------------------------------------------------------------------------

-- TODO Const ab

--------------------------------------------------------------------------------

-- TODO Instances for Proxy a.

--------------------------------------------------------------------------------

-- TODO Instances for State a b.

--------------------------------------------------------------------------------

instance Monad (State s) where
  State ms >>= toNewMs = State (\s'' -> p (ms s'')) where
    p (s', v) = runState (toNewMs v) s'

--------------------------------------------------------------------------------  

-- TODO Instances for Cont a b.
--------------------------------------------------------------------------------  

-- TODO Instances for Star m a b, given instances for m.
--------------------------------------------------------------------------------  

-- TODO Instances for Costar m a b.
--------------------------------------------------------------------------------  

-- TODO Instances for Yoneda m a, given instances for m.
--------------------------------------------------------------------------------  

-- TODO Instances for Coyoneda m a.
--------------------------------------------------------------------------------  
