module Week5.Exercise3 where

import Text.Show.Functions

import Data.Int (Int8 (..), Int16 (..))
import Data.Set (Set (..))
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.State.Strict as State

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
collatzBound = Just 2

data Variable = Value | Counter | Cache
  deriving Show

data Problem = Loop | Bound Variable
  deriving Show

--------------------------------------------------------------------------------

runCollatz :: CollatzType a -> Either Problem (Int, Set a)
runCollatz ts = runExcept (runReaderT (runStateT ts Set.empty) collatzBound)

--------------------------------------------------------------------------------

type CollatzType a = StateT (Set a) (ReaderT (Maybe Int) (Except Problem)) Int

checkCollatz :: Intlike a => a -> CollatzType a
checkCollatz = let
  f :: Intlike a => a -> CollatzType a
  f n = do
    ps <- get --Get the Set inside the State
    if Set.member n ps then --Check if n is inside the Set
      (lift . lift) (throwE Loop) else do --if it is, throw exception
      ms <- lift ask --get Maybe Int inside the Reader
                     -- ms represents another max bound to Set size
      case ms of
        Nothing -> if Set.size ps >= maxBound then --if Set is too big, throw exception
          (lift . lift) (throwE (Bound Cache)) else
          put (Set.insert n ps)
        Just m -> if Set.size ps >= m then -- if Set is too big, return pure
          pure () else
          put (Set.insert n ps)
      if abs n == 1 then
        pure 0 else
        if not (safeToCollatz n) then
          (lift . lift) (throwE (Bound Value)) else let
          p = collatz n in do
          q <- f p
          if not (safeToCount q) then
            (lift . lift) (throwE (Bound Counter)) else
            pure (count q) in
  \ n -> (State.liftCatch . Reader.liftCatch) catchE (f n) $ \ e -> case e of
    Bound Cache -> mapStateT (local (const collatzBound)) (f n)
    _ -> (lift . lift) (throwE e)


