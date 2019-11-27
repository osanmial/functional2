{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise1 where
import Prelude hiding (Foldable, foldMap, foldr, Traversable, traverse)
import Utility.Simple
-- In the following I put to do list to complete this exercies. 
-- To do that I choose that we have to implement on  of
-- the following function for traverse or sequenceA where I thinki we 
-- do not need to do both (one is enough)

-- TODO: 
-- traverse or  sequenceA for []
-- traverse or sequenceA for NonEmpty
-- foldMap for Map
-- traverse or sequenceA for Map
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
  sequenceA :: Applicative f => t (f a) -> f (t a)

--Instances for Bool.
-- Bool has a wrong kind

---------------------------------------------------------------------

instance Foldable Maybe where
  foldMap f (Just x) = f x
  foldMap _ Nothing  = mempty 

instance Traversable Maybe where
  traverse f (Just x) = Just <$> f x
  traverse _ Nothing  = pure Nothing
  sequenceA = undefined
---------------------------------------------------------------------

instance Foldable (Either a) where
    foldMap f  e = case e of 
            Right a -> f a
            Left _ -> mempty 

instance Traversable (Either a) where
  traverse f (Right x) = Right <$> f x
  traverse _ (Left x)  = pure (Left x)
  sequenceA=undefined
---------------------------------------------------------------------

instance Foldable ((,) a) where
  foldMap f (_, x) = f x
instance Traversable ((,) a) where
  traverse f (x,y) =  (\ z -> (,) x z) <$> f y
  sequenceA=undefined
---------------------------------------------------------------------
--Instances for Endo a.
--No instance of foldmap for endo as we would require something
--with the strength of profunctor to alter its type.
---------------------------------------------------------------------

--instance Foldable ((->) a)
--I cannot get rid of the input of the function to get access to the wrapped value I actually wish to handle. And so I can't convert this to a aritrary monoid.
  
--OP does not work with foldMap as we would require a contravariant like function to alter the type and we have just a normal one.

---------------------------------------------------------------------

--Instances for ().
-- Unit has wrong kind

---------------------------------------------------------------------

instance Foldable [] where
  foldMap f (x:xs) = f x <> foldMap f xs
  foldMap _ []     = mempty
--traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
instance Traversable [] where
    traverse f (x:xs)= undefined 
    sequenceA = undefined
---------------------------------------------------------------------

instance Foldable NonEmpty where
  foldMap f (x :| xs) = f x <> foldMap f xs


instance Traversable NonEmpty where
    traverse f (x:|xs)= undefined 
    sequenceA = undefined
---------------------------------------------------------------------

--Instances for Void.
--Void has a wrong kind

---------------------------------------------------------------------
--Instances for IO a. it is not doable because ther is no way to extract Monoid from IO
-- Since it is not Foldable so it is not Traversable 
--  foldMap :: Monoid m => (a -> m) -> t a -> m
--instance Foldable IO where
--  foldMap f i=do 
--    x <- i
--    return (f  x)
-- 
---------------------------------------------------------------------
--Instances for Map k a.
instance Foldable (Map k) where
  foldMap f x = undefined
  
instance Traversable (Map k) where
    traverse = undefined 
    sequenceA = undefined

