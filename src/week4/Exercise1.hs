module Week4.Exercise1

import Week3.Exercise1

class Monad
-- (->) a b

-----------------------------------------------------------------

-- Endo not applicative so not monad.

-----------------------------------------------------------------

-- Op not a functor so not an applicative so not a monad

-----------------------------------------------------------------

instance Monad Arrow a =
return = pure

