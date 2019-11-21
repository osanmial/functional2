module Week4.Exercise2 where

import Prelude hiding (Monad, return, (>>=), (>>))
import Week4.Exercise1
import Week3.Exercise2
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
    return x = Identity x
    (Identity x) >>= f = f x 

--------------------------------------------------------------------------------
-- The idea in bind is to combine monadic value ma containing values of type a 
--  and a function which operates on a value v of type a, returning the monadic 
--  value mb. 

--One can do Monads compose, but the result might not be a monad. 
-- There is a kind of statment says : "Applicatives compose, monads don't."
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
--In continuation-passing style, computations are built up from sequences 
--of nested continuations, terminated by a final continuation (often id) which 
--produces the final result. 

-- Only the last part  runCont (gs i) h is not clear to me since : 
-- Cont f>>=  gs = Cont $ \ h -> f $ \ i ->  _ 
-- f: (a1->a) ->a
-- gs ::  a1-> Cont a b
-- h:: b ->a
-- i:: a1 -> a 
-- _ :: a
----------------------------------------------------------------------------------

instance Monad (Cont a )  where
  Cont f  >>=  gs = Cont $ \ h -> f $ \ i -> runCont (gs i) h 




instance Applicative (Cont a) where
    pure x = Cont (\f-> f x) 
    Cont  f <*> Cont g = Cont $ \ h -> f $ \ k -> g $ \ x -> h (k x)
    
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
   
