{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise2 where
import Prelude hiding (Monad, return, (>>=), (>>))
import Data.List.NonEmpty as Esko hiding (map)

import GHC.Base hiding (Monad, (>>=), (>>))

--   a -> ExceptT Problem (ReaderT (Maybe Int) (State (Set a))) b  -- Sampsa
--   a -> ExceptT Problem  (State (Set a) (ReaderT (Maybe Int)) ) b
-- a- > StateT (Set a) (ReaderT (Maybe Int) (Except Problem)) b  --Sampsa 
-- a -> StateT (Set a)    (Except Problem (ReaderT (Maybe Int) )) b
-- a -> ReaderT (Maybe Int) ( Except Problem (StateT (Set a)  ))
-- a -> ReaderT (Maybe Int) ( StateT (Set a)  (Except Problem))