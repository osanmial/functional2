{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
module Week7.Exercise1 where

fix :: (a -> a) -> a
fix f = f (fix f)

id' :: a -> a
id' = fix (const id)

reverse' :: [a] -> [a]
reverse' xs = fix reverse'' xs []
reverse'' r [] ys = ys
reverse'' r (x:xs) ys = r xs (x:ys)

--f
  -- where
  --   f r (x:xs) = (r xs) ++ [x]
  --   f _ []     = []

(++:) :: [a] -> [a] -> [a]
(++:) as bs = fix (++::) as bs

(++::) :: ([a] -> [a]-> [a]) -> [a] -> [a] -> [a]
(++::) r [] bs = bs
(++::) r (a:as) bs = a : r as bs

repeat' :: a -> [a]
repeat' x = fix (x:) -- repeat''

repeat'' :: (t -> [t]) -> t -> [t]
repeat'' r  a = a : r a

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' = fix foldr'' --foldr''

foldr'' :: ((a -> b -> b) -> b -> [a] -> b) -> (a -> b -> b) -> b -> [a] -> b
foldr'' r f e [] = e
foldr'' r f e (x:xs) = f x (r f e xs)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' = fix unfoldr''

unfoldr'' :: ((b -> Maybe (a, b)) -> b -> [a]) -> (b -> Maybe (a, b)) -> b -> [a]
unfoldr'' r f b = case f b of
    Just (a,b) -> a:r f b 
    Nothing -> []

fix' :: (a -> a) -> a
fix' = fix fix''

fix'' r f = f (r f)


