module Week4.Exericse2 where


import Week4.Exercise1

instance Monad (State m) where
  stateA >>= toNewState = undefined
