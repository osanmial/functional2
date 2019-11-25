{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise1 where
import Prelude hiding (Foldable, foldMap, foldr)
import Data.List.NonEmpty as Esko hiding (map)
import Data.Monoid.Endo


class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

 

--Instances for Bool.
-- Bool has a wrong kind

---------------------------------------------------------------------

instance Foldable Maybe where
  foldMap f (Just x) = f x
  foldMap _ Nothing  = mempty 


---------------------------------------------------------------------
--Instances for Either a b.
instance Foldable (Either a) where
    foldMap f  e = case e of 
            Right a -> f a
            Left _ -> mempty 
---------------------------------------------------------------------

instance Foldable ((,) a) where
  foldMap f (_, x) = f x

---------------------------------------------------------------------
--Instances for Endo a.
---------------------------------------------------------------------
--Instances for (->) a b and Op a b.
---------------------------------------------------------------------

--Instances for ().
-- Unit has wrong kind

---------------------------------------------------------------------

instance Foldable [] where
  foldMap f (x:xs) = f x <> foldMap f xs
  foldMap _ []     = mempty

---------------------------------------------------------------------

instance Foldable NonEmpty where
  foldMap f (x :| xs) = f x <> foldMap f xs

---------------------------------------------------------------------

--Instances for Void.
-- Void has a wrong kind

---------------------------------------------------------------------
--Instances for IO a.
instance Foldable IO where
  foldMap f x = undefined
  
---------------------------------------------------------------------
--Instances for Map k a.
instance Foldable (Map k) where
  foldMap f x = undefined
