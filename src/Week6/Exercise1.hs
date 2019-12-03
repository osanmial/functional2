module Week6.Exercise1 where

import Data.Functor.Compose -- Why imported?
import Data.Functor.Const
import Data.Functor.Identity

newtype WrappedTraversable m a = WrapTraversable {unwrapTraversable :: m a}
instance Traversable m => Functor (WrappedTraversable m) where
  --fmap f  (WrapTraversable xs)=  WrapTraversable $ (runIdentity.traverse (Identity . f)) xs
    fmap f  = runIdentity.traverse (Identity . f) 
  
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- runIdentity :: Identity a -> a
  -- identity :: a -> identity a
  -- f :: a -> b 
  -- Identity:: a -> identity b
  
  -- this also works but why the above doesn't need to unwrap? Because newtype I guess?
  -- fmap f (WrapTraversable xs) =  WrapTraversable . runIdentity . traverse (Identity . f) $ xs

instance Traversable m => Foldable (WrappedTraversable m) where
   -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
   -- The below is working:
   --foldMap f  (WrapTraversable xs)= WrapTraversable $ (getConst . traverse (Const . f) ) xs
     foldMap f  = getConst . traverse (Const . f) 
  -- Const :: a -> Const a b
  -- getConst :: Const a b -> a
  -- f :: a -> m 
  

instance Traversable m => Traversable (WrappedTraversable m) where
  sequenceA = traverse id
  traverse k = traverse k
--  traverse k (WrapTraversable xs) =  WrapTraversable <$> traverse k xs
