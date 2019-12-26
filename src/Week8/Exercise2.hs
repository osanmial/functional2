{-# OPTIONS_GHC -XDeriveDataTypeable  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Week8.Exercise2 where

import Control.Exception
import Control.Monad.Catch
import Data.Stream.Infinite (Stream (..))
import qualified Data.Tree as Rose (Forest (..), Tree (..))
import qualified Data.Tree.Binary.Inorder as Binary (Tree (..))
import Week8.Exercise1
import Data.Bifunctor.Join
import Data.Typeable

data ZipperEnd = ZippersDeadEnd
     deriving (Show, Typeable)

instance Exception ZipperEnd

--type A := Just a | Nothing
-- -> a + 1 
--context from derivative := 1
--type ZA := (a, ())

stepperMaybe :: MonadThrow m => Maybe a -> m (a, ())
stepperMaybe (Just a) = pure (a,())
stepperMaybe (Nothing) = throwM ZippersDeadEnd

stepMaybe :: MonadThrow m => (a, ()) -> m (a, ())
stepMaybe a = pure a

--type A = Join (,) a
{-

Join a = {runJoin :: p a a }
Join (,) a -> (a,a) 
type (a×a)
-> a^2
context:
2a
==> (Bool, a)

type ZA (a, (Bool, a))

-}

stepperJoin :: MonadThrow m => Join (,) a -> m (a, (Bool, a))
stepperJoin jaa = pure (fst (runJoin jaa), (True, snd $ runJoin jaa ))

stepJoin :: MonadThrow m => (a, (Bool, a)) -> m (a, (Bool, a))
stepJoin (right, (False, other)) =  pure (other, (True, right))
stepJoin (left, (True, other)) =  pure (other, (False, left))

{-
A := [a]
context := [a]×[a]
ZA := (a,[a]×[a])
-}

stepperList :: MonadThrow m => [a] -> m (a,([a], [a]))
stepperList (x:xs) = pure (x, ([],xs))
stepperList [] = throwM ZippersDeadEnd

stepList :: MonadThrow m => (a,([a], [a])) -> m (a,([a],[a]))
stepList (a,(as, b:bs)) = pure (b,(a:as,bs))
stepList (_,(_, [])) = throwM ZippersDeadEnd

stepperStream :: MonadThrow m => Stream a -> m (a, ([a],Stream a))
stepperStream (a :> as) = pure (a, ([],as))

stepStream :: MonadThrow m => (a, ([a],Stream a)) -> m (a, ([a],Stream a))
stepStream (a, (as, b :> bs))= pure (b, (a:as, bs))

{-
()
D(μy.F) ↦ μz.(((D_x F) |y=μy.F) + ((D_y F) |y=μy.F) × z)
D(F|y=S) ↦ D_x(F|y=S) + D_y(F|y=S) × D_x S


A := Leaf | Node (Tree a) a (Tree a)
μy (1 + (y×y×a))
(1 + (ay^2))
D >> y^2
(1 + (y×y×a)) × (1 + (y×y×a))
context 
-}

--stepBTree :: MonadThrow m => Binary.Tree a -> m (a, ([a],Stream a))
--stpeBTree = undefined
