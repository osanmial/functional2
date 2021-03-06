{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Week5.Exercise1 where
import Prelude hiding (Foldable, foldMap, foldr, Traversable, traverse,sequenceA)
import Utility.Simple

class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id

---------------------------------------------------------------------

--Instances for Bool.
-- Bool has a wrong kind

---------------------------------------------------------------------

instance Foldable Maybe where
  foldMap f (Just x) = f x
  foldMap _ Nothing  = mempty 

instance Traversable Maybe where
  traverse f (Just x) = Just <$> f x
  traverse _ Nothing  = pure Nothing
  
---------------------------------------------------------------------

instance Foldable (Either a) where
    foldMap f  e = case e of 
            Right a -> f a
            Left _ -> mempty 

instance Traversable (Either a) where
  traverse f (Right x) = Right <$> f x
  traverse _ (Left x)  = pure (Left x)
  
---------------------------------------------------------------------

instance Foldable ((,) a) where
  foldMap f (_, x) = f x
instance Traversable ((,) a) where
  traverse f (x, y) =  (\z -> (x, z)) <$> f y
  
---------------------------------------------------------------------

--Instances for Endo a.
-- No instance of foldmap for endo as we would require something
-- with the strength of profunctor to alter its type.

---------------------------------------------------------------------

--Instance Foldable ((->) a)
-- I cannot get rid of the input of the function to get access to the wrapped value I actually wish to handle. And so I can't convert this to a aritrary monoid.
  
-- OP does not work with foldMap as we would require a contravariant like function to alter the type and we have just a normal one.

---------------------------------------------------------------------

--Instances for ().
-- Unit has wrong kind

---------------------------------------------------------------------

instance Foldable [] where
  foldMap f (x:xs) = f x <> foldMap f xs
  foldMap _ []     = mempty
instance Traversable [] where
  sequenceA (f:fs) =  (:) <$>  f  <*> sequenceA fs
    
---------------------------------------------------------------------

instance Foldable NonEmpty where
  foldMap f (x :| xs) = f x <> foldMap f xs

instance Traversable NonEmpty where
        sequenceA (f :|fs)= (:|) <$>  f  <*> sequenceA fs

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

---------------------------------------------------------------------

--Instances for Map k a.
instance Foldable (Map k) where
  foldMap f mp = foldMap f (elems mp)
  
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b) 
--  sequenceA :: Applicative f => t (f a) -> f (t a)
instance Traversable (Map k) where
    traverse f mp = traverseWithKey (\_ -> f) mp

