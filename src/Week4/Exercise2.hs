module Week4.Exercise2 where

import Prelude hiding (Monad, return, (>>=), (>>))
import Week4.Exercise1
import Utility.Complex


instance (Monad m , Monad n)=>Monad (Product m n)  where
    (Pair x1 y1) >>= f=  Pair (x1 >>= (fst'.f)) (y1 >>= (snd'.f))
            where 
                fst' (Pair fx _)= fx
                snd' (Pair _ fy)= fy

--------------------------------------------------------------------------------

-- Sum m n a is not applicative so no instance

--------------------------------------------------------------------------------

instance Monad Identity where
  (Identity x) >>= f = f x 

--------------------------------------------------------------------------------

-- instance Monad Compose 
-- The idea in bind is to combine monadic value ma containing values of type a 
--  and a function which operates on a value v of type a, returning the monadic 
--  value mb. 

--One can do Monads compose, but the result might not be a monad. 
-- There is a kind of statment says : "Applicatives compose, monads don't."

--------------------------------------------------------------------------------

--Giving a value to 'f' might be impossible
instance Monoid a => Monad (Const a) where
  Const x >>= f = undefined --f 

--------------------------------------------------------------------------------

instance Monad Proxy where
  _ >>= _ = Proxy

--------------------------------------------------------------------------------

instance Monad (State s) where
  State ms >>= toNewMs = State (\s'' -> p (ms s'')) where
    p (s', v) = runState (toNewMs v) s'

--------------------------------------------------------------------------------  

-- TODO Instances for Cont a b.

--------------------------------------------------------------------------------  

-- TODO Instances for Star m a b, given instances for m.
instance Monad f => Monad (Star f d) where
  Star x >>= fun = Star a
    where
      a = undefined-- \y -> do
        --b <- x y
        --runStar (fun b) y
        
      
--------------------------------------------------------------------------------  

instance Monad (Costar f c) where
  Costar x >>= fun = Costar $ \a -> runCostar ((fun . x) a ) a

--------------------------------------------------------------------------------  

-- TODO Instances for Yoneda m a, given instances for m.
--------------------------------------------------------------------------------  

-- TODO Instances for Coyoneda m a.
--------------------------------------------------------------------------------  
