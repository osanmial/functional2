module Week4.Exercise2 where

import Prelude hiding (Monad, return, (>>=), (>>))
import Week4.Exercise1
import Utility.Complex



--Sum m n a is not applicative

--------------------------------------------------------------------------------

instance (Applicative m, Applicative n) => Monad (Product m n) where
  _ >>= _ = undefined
  
