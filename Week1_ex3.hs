module Week1_ex3 where

import Prelude hiding ((==), Eq, Ord, compare, Enum, pred)
import Control.Monad.State
import Data.List

-- class Transaction k m a where
--    recall :: k -> m a
--    store :: k -> a -> m ()

data Transaction k m a = Trans {recall :: k -> m a, store :: k -> a -> m ()}

data Enum a = Enum {predSomething :: a -> a}

enumInt :: Enum Int
enumInt = Enum { predSomething = \x -> x - 1}

pred :: Int -> Int
pred = (predSomething enumInt)

data Eq a = Eq { equal :: a -> a -> Bool }

eqInt :: Eq Int
eqInt = Eq { equal = f }
  where
    f :: Int -> Int -> Bool
    f 0 0 = True
    f _ 0 = False
    f 0 _ = False
    f x y = f (x - y) 0 

(==) :: Int -> Int -> Bool
x == y = (equal eqInt) x y

data Ord a = Ord { compareSomething :: a -> a -> Ordering }

ordInt :: Ord Int
ordInt = Ord {compareSomething = f}
  where
    f :: Int -> Int -> Ordering
    f x y
      | x ==y                  = EQ
      | (x - y) == (abs (x - y)) = GT
      | otherwise              = LT

compare :: Int -> Int -> Ordering
compare = (compareSomething ordInt)

data Cursor a = WayBefore | Before [a] | At [a] a [a] | After [a] | WayAfter


navigate :: Int -> [a] -> Cursor a
navigate n xs = case compare n (pred 0) of
  LT -> WayBefore
  EQ -> Before xs
  GT -> let
    f :: Int -> [a] -> Cursor a
    f p []
      | p == 0 = After []
      | otherwise = WayAfter
    f p (z : zs)
      | p == 0 = At [] z zs
      | otherwise = case f (pred p) zs of
        At bs a as -> At (z : bs) a as
        After bs -> After (z : bs)
        c -> c in
    f n xs

database :: FilePath
database = "/tmp/database"

transIntIOStr :: Transaction Int IO String
transIntIOStr = Trans {recall = f, store =  g} where
  
  f :: Int -> IO String
  f i = do
    s <- readFile database
    case navigate i (lines s) of
      At _ c _ -> pure c
      _ -> ioError (userError "Invalid line number")
      
  g :: Int -> String -> IO ()
  g i x = do
    s <- readFile database
    case navigate i (lines s) of
      Before rs -> writeFile database
        (unlines (x : rs))
      At ls _ rs -> writeFile database
        (unlines (ls <> (x : rs)))
      After ls -> writeFile database
        (unlines (ls <> [x]))
      _ -> ioError (userError "Invalid line number")


-- instance Transaction Int IO String where
--   recall i = do
--     s <- readFile database
--     case navigate i (lines s) of
--       At _ c _ -> pure c
--       _ -> ioError (userError "Invalid line number")
--   store i x = do
--     s <- readFile database
--     case navigate i (lines s) of
--       Before rs -> writeFile database
--         (unlines (x : rs))
--       At ls _ rs -> writeFile database
--         (unlines (ls <> (x : rs)))
--       After ls -> writeFile database
--         (unlines (ls <> [x]))
--       _ -> ioError (userError "Invalid line number")

transIntStateSring :: Transaction Int (State (Int -> String)) String
transIntStateSring = Trans reca sto where
  reca :: Int -> (State (Int -> String) String)  
  reca i = gets $ \ f -> f i
  sto :: Int -> String -> (State (Int -> String)) ()  
  sto i x = modify $ \ f j -> if j == i then x else f j

-- instance Transaction Int (State (Int -> String)) String where
--   recall i = gets $ \ f -> f i
--   store i x = modify $ \ f j -> if j == i then x else f j

work :: IO String
work = do
  (store transIntIOStr) (0 :: Int) "zero"
  (store transIntIOStr) (1 :: Int) "one"
  (store transIntIOStr) (2 :: Int) "two"
  x <- (recall transIntIOStr) (0 :: Int)
  y <- (recall transIntIOStr) (1 :: Int)
  z <- (recall transIntIOStr) (2 :: Int)
  pure (intercalate " " [x, y, z])

mock :: String
mock = let
  f :: Int -> String
  f _ = mempty in
  flip evalState f $ do
  (store transIntStateSring) (0 :: Int) "zero"
  (store transIntStateSring) (1 :: Int) "one"
  (store transIntStateSring) (2 :: Int) "two"
  x <- (recall transIntStateSring) (0 :: Int)
  y <- (recall transIntStateSring) (1 :: Int)
  z <- (recall transIntStateSring) (2 :: Int)
  pure (intercalate " " [x, y, z])
