{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
module Week7.Exercise1 where

fix :: (a -> a) -> a
fix f = f (fix f)

id' :: a -> a
id' = fix (const id)

repeat' :: a -> [a]
repeat' x = fix (x:)

reverse' :: [a] -> [a]
reverse' = fix f
  where
    f r (x:xs) = (r xs) ++ [x]
    f _ []     = []


(++:) :: [a] -> [a] -> [a]
(++:) as bs = fix (++::) as bs

(++::) :: ([a] -> [a]-> [a]) -> [a] -> [a] -> [a]
(++::) r [] bs = bs
(++::) r (a:as) bs = a : r as bs


foldr':: (a -> b -> b) -> b -> [a] -> b
foldr' = fix foldF
  where
    foldF r f e [ ] = e
    foldF r f e (x:xs )=  f  x (r f e xs)


