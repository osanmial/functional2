module Week7.Exercise1 where

fix :: (a -> a) -> a
fix f = f (fix f)

id' = fix (const id)

repeat' :: a -> [a]
repeat' x = fix (x:)

reverse' :: [a] -> [a]
reverse' = fix f
  where
    f r (x:xs) = (r xs) ++ [x]
    f _ []     = []

fix' = fix (const fix)
