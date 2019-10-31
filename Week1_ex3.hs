module Week1_ex1 where

import Control.Monad.State
import Data.List

class Transaction k m a where
  recall :: k -> m a
  store :: k -> a -> m ()

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

instance Transaction Int IO String where
  recall i = do
    s <- readFile database
    case navigate i (lines s) of
      At _ c _ -> pure c
      _ -> ioError (userError "Invalid line number")
  store i x = do
    s <- readFile database
    case navigate i (lines s) of
      Before rs -> writeFile database
        (unlines (x : rs))
      At ls _ rs -> writeFile database
        (unlines (ls <> (x : rs)))
      After ls -> writeFile database
        (unlines (ls <> [x]))
      _ -> ioError (userError "Invalid line number")

instance Transaction Int (State (Int -> String)) String where
  recall i = gets $ \ f -> f i
  store i x = modify $ \ f j -> if j == i then x else f j

work :: IO String
work = do
  store (0 :: Int) "zero"
  store (1 :: Int) "one"
  store (2 :: Int) "two"
  x <- recall (0 :: Int)
  y <- recall (1 :: Int)
  z <- recall (2 :: Int)
  pure (intercalate " " [x, y, z])

mock :: String
mock = let
  f :: Int -> String
  f _ = mempty in
  flip evalState f $ do
  store (0 :: Int) "zero"
  store (1 :: Int) "one"
  store (2 :: Int) "two"
  x <- recall (0 :: Int)
  y <- recall (1 :: Int)
  z <- recall (2 :: Int)
  pure (intercalate " " [x, y, z])
