module Week4.Exercise2 where

import Prelude hiding (Monad, return, (>>=), (>>))
import Week4.Exercise1
import Utility.Complex

instance (Applicative m, Applicative n) => Monad (Product m n) where
  _ >>= _ = undefined

--------------------------------------------------------------------------------

-- Sum m n a is not applicative so no instance

--------------------------------------------------------------------------------

-- TODO product

--------------------------------------------------------------------------------

-- TODO Identity

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
