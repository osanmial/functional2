{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise1 where
import Prelude hiding (Foldable,Monad, return, (>>=), (>>))
import Data.List.NonEmpty as Esko hiding (map)

import GHC.Base hiding (Monad, (>>=), (>>))


class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
 

--Instances for Bool.

---------------------------------------------------------------------
--Instances for Maybe a.

---------------------------------------------------------------------
--Instances for Either a b.
instance Foldable (Either a) where
    foldMap f  e = case e of 
            Right a -> f a
            Left _ -> mempty 
---------------------------------------------------------------------
--Instances for (,) a b.
---------------------------------------------------------------------
--Instances for Endo a.
---------------------------------------------------------------------
--Instances for (->) a b and Op a b.
---------------------------------------------------------------------
--Instances for ().
---------------------------------------------------------------------
--Instances for [] a.
---------------------------------------------------------------------
--Instances for NonEmpty a.
---------------------------------------------------------------------
--Instances for Void.
---------------------------------------------------------------------
--Instances for IO a.
---------------------------------------------------------------------
--Instances for Map k a.
