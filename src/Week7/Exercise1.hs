{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
module Week7.Exercise1 where

fix :: (a -> a) -> a
fix f = f (fix f)

id' :: a -> a
id' = fix f where
  f b a = a

(++:) :: [a] -> [a] -> [a]
(++:) as bs = fix (++::) as bs

(++::) :: ([a] -> [a]-> [a]) -> [a] -> [a] -> [a]
(++::) r [] bs = bs
(++::) r (a:as) bs = a : r as bs


