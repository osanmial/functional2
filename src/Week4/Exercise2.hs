{-#LANGUAGE RebindableSyntax #-}
module Week4.Exercise2 where

import Prelude hiding (Monad, return, (>>=), (>>))
import Week4.Exercise1
import Week3.Exercise2
import Utility.Complex  


--------------------------------------------------------------------------------

-- Sum m n a is not applicative so no instance

--------------------------------------------------------------------------------

instance (Monad m , Monad n)=>Monad (Product m n)  where
    (Pair x1 y1) >>= f=  Pair (x1 >>= (fst'.f)) (y1 >>= (snd'.f))
            where 
                fst' (Pair fx _)= fx
                snd' (Pair _ fy)= fy

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
-- There is no monad for Const because it breaks the monad law of the  Left identity this should be 
-- If we have function 
-- f = \ Const ( x + 1 ) 
-- Let us check if 
-- return x >>= f =? f x 
-- LHS = Const mempty
-- RHS = Const (x+1 )

--------------------------------------------------------------------------------

instance Monad Proxy where
  _ >>= _ = Proxy

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

instance Monad (Cont a )  where
  Cont f  >>=  gs = Cont $ \ h -> f $ \ i -> runCont (gs i) h 


instance Applicative (Cont a) where
    pure x = Cont (\f-> f x) 
    Cont  f <*> Cont g = Cont $ \ h -> f $ \ k -> g $ \ x -> h (k x)
    
--------------------------------------------------------------------------------  
instance Monad f => Monad (Star f a) where
  Star fss >>= g  = Star $ \ h -> do 
   a <- fss h    
   runStar (g a) h 
    
--------------------------------------------------------------------------------  

instance Monad (Costar f c) where
  Costar x >>= fun = Costar $ \a -> runCostar ((fun . x) a ) a

--------------------------------------------------------------------------------  

-- Yoneda f >>= g = Yoneda \ h -> _ 
-- f :: forall b. (a -> b) -> f b
-- g :: a -> Yoneda f b
-- h :: b1 -> b 
-- _ :: f b1 
-- id :: a -> a 
-- runYoneda :: Yoneda f b  ->( b -> b1 ) -> f b1 
instance Monad m => Monad (Yoneda m) where
  Yoneda f>>= g = Yoneda  $ \ h -> do 
    x <- f id  -- x :: a 
    runYoneda (g x) h -- g x :: Yoneda f b 
 
--------------------------------------------------------------------------------  
-- (Coyoneda  f x) >>= g  =$ _
-- g :: a -> Coyoneda m b 
-- x :: m b1 
-- f :: b1 -> a
-- _ :: Coyoneda m b
instance Monad m => Monad (Coyoneda m) where
  Coyoneda f x >>= g =liftCoyoneda' $ do 
     v <- x                                -- v :: b1
     lowerCoyoneda' ( g (f v))     -- f v :: a 
                                              -- g (f v ):: Coyoneda m b
                                             -- All the expression :: m b
-- I have to ask Sampsa about second approch 
--  Coyoneda  f x >>=  g = liftCoyoneda' $ lowerCoyoneda'  $ g $ do --  g (f v):: Coyoneda m b
--    v <- x    -- v :: b1 
--    f v  -- f v :: a   
----------------------------------------------------------------------------------------
-- Helper functions from Sampsa
liftCoyoneda' :: m a -> Coyoneda m a
liftCoyoneda'= Coyoneda id

lowerCoyoneda':: Functor m => Coyoneda m a -> m a
lowerCoyoneda' (Coyoneda f xs) = fmap f xs
    


--------------------------------------------------------------------------------
