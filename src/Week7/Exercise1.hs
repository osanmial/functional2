module Week7.Exercise1 where

fix :: (a -> a) -> a
fix f = f (fix f)
