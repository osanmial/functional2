{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise1 where
import Prelude hiding (Foldable, foldMap, foldr)
import Utility.Simple


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
--No instance of foldmap for endo as we would require something
--with the strength of profunctor to alter its type.
---------------------------------------------------------------------

--instance Foldable ((->) a)
-- I cannot get rid of the input of the function to get access to the wrapped value I actually wish to handle. And so I can't convert this to a aritrary monoid.
  
-- OP does not work with foldMap as we would require a contravariant like function to alter the type and we have just a normal one.

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
--Void has a wrong kind

---------------------------------------------------------------------
--Instances for IO a.
instance Foldable IO where
  foldMap f x = undefined
  
---------------------------------------------------------------------
--Instances for Map k a.
instance Foldable (Map k) where
  foldMap f x = undefined

