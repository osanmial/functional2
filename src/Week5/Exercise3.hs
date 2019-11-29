{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Week5.Exercise3 where

import Text.Show.Functions

import Control.Monad.Cont.Class
import Control.Monad.Except
import Control.Monad.Reader as Reader
import Control.Monad.State.Strict as State
import Data.Int (Int16 (..), Int8 (..))
import Data.Set (Set (..))
import qualified Data.Set as Set

type Intlike a = (Bounded a, Integral a)

count :: Intlike a => a -> a
count n = 1 + n

safeToCount :: Intlike a => a -> Bool
safeToCount n = n < maxBound

collatz :: Intlike a => a -> a
collatz n
  | even n = n `div` 2
  | otherwise = 1 + 3 * n

safeToCollatz :: Intlike a => a -> Bool
safeToCollatz n
  | even n = True
  | otherwise = n > minBound `div` 3 && n < maxBound `div` 3

collatzBound :: Intlike a => Maybe a
collatzBound = Nothing

data Variable = Value | Counter | Cache
  deriving Show

data Problem = Loop | Bound Variable
  deriving Show

--------------------------------------------------------------------------------

type CollatzRunType a = StateT (Set a) (ReaderT (Maybe Int) (Except Problem)) Int

runCollatz :: CollatzRunType a -> Either Problem (Int, Set a)
runCollatz ts = runExcept (runReaderT (runStateT ts Set.empty) collatzBound)

--------------------------------------------------------------------------------

type CollatzType m a = (MonadError Problem m,
                        MonadReader (Maybe Int) m,
                        MonadState (Set a) m)

checkCollatz :: (CollatzType m a, Intlike a) => a -> m a
checkCollatz = let
  f :: (CollatzType m a, Intlike a) => a -> m a
  f n = do
    ps <- get
    if Set.member n ps then
      throwError Loop else do
      ms <- ask
      case ms of
        Nothing -> if Set.size ps >= maxBound then
          throwError (Bound Cache) else
          put (Set.insert n ps)
        Just m -> if Set.size ps >= m then
          pure () else
          put (Set.insert n ps)
      if abs n == 1 then
        pure 0 else
        if not (safeToCollatz n) then
          throwError (Bound Value) else let
          p = collatz n in do
          q <- f p
          if not (safeToCount q) then
            throwError (Bound Counter) else
            pure (count q) in
    \n -> catchError (f n) $ \e -> case e of
      Bound Cache -> local (const collatzBound) (f n)
      _           -> throwError e


