{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise1 where
import Prelude hiding (Monad, return, (>>=), (>>))
import Data.List.NonEmpty as Esko hiding (map)

import GHC.Base hiding (Monad, (>>=), (>>))


class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b 
