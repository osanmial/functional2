{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ViewPatterns #-}

module Week6.Exercise2 where

class Covariant m where
  (<&>) :: m a -> (a -> b) -> m b
infixl 1 <&>

class Covariant m => Monoidal m where
  unit :: m ()
  (>*<) :: m a -> m b -> m (a, b)
infix 5 >*<

class Monoidal m => Triad m where
  join :: m (m a) -> m a

newtype WrappedFunctor m a = WrapFunctor {unwrapFunctor :: m a}

instance Covariant m => Functor (WrappedFunctor m) where
  fmap f (WrapFunctor xs) =WrapFunctor $  xs <&> f  

instance Functor m => Covariant (WrappedFunctor m) where
  WrapFunctor xs <&> f = WrapFunctor $ fmap f xs

newtype WrappedCovariant m a = WrapCovariant {unwrapCovariant :: m a}

instance Covariant m => Functor (WrappedCovariant m) where
  fmap f (WrapCovariant xs) = WrapCovariant
    (unwrapFunctor (fmap f (WrapFunctor xs)))

instance Functor m => Covariant (WrappedCovariant m) where
  WrapCovariant xs <&> f = WrapCovariant (unwrapFunctor (WrapFunctor xs <&> f))

newtype WrappedApplicative m a = WrapApplicative {unwrapApplicative :: m a}

instance Covariant m => Functor (WrappedApplicative m) where
  fmap f (WrapApplicative xs) = WrapApplicative
    (unwrapFunctor (fmap f (WrapFunctor xs)))

instance Functor m => Covariant (WrappedApplicative m) where
  WrapApplicative xs <&> f = WrapApplicative
    (unwrapFunctor (WrapFunctor xs <&> f))

instance Monoidal m => Applicative (WrappedApplicative m) where
  pure x = WrapApplicative $ unit <&> (const x)
  (WrapApplicative mf) <*> (WrapApplicative ma) = WrapApplicative $ (ma >*< mf) <&> (\(a,fab)-> fab a)

--  unit :: m ()
-- (>*<) :: m a -> m b -> m (a, b)
-- (<*>) :: m (a -> b) -> m a -> m b
-- (<&>) :: m a -> (a -> b) -> m b
-- (>*<' :: m a -> m (a -> b) -> m (a, (a -> b))

instance Applicative m => Monoidal (WrappedApplicative m) where
  unit = WrapApplicative $ pure ()
  WrapApplicative ma >*< WrapApplicative mb = WrapApplicative $ (,) <$> ma <*> mb

{-
(<$>) :: (a -> b) -> f a -> f b
(<*>) :: m (a -> b) -> m a -> m b
unit :: m ()
-- (>*<) :: m a -> m b -> m (a, b)

-}  

newtype WrappedMonoidal m a = WrapMonoidal {unwrapMonoidal :: m a}

instance Covariant m => Functor (WrappedMonoidal m) where
  fmap f (WrapMonoidal xs) = WrapMonoidal
    (unwrapFunctor (fmap f (WrapFunctor xs)))

instance Functor m => Covariant (WrappedMonoidal m) where
  WrapMonoidal xs <&> f = WrapMonoidal (unwrapFunctor (WrapFunctor xs <&> f))

instance Monoidal m => Applicative (WrappedMonoidal m) where
  pure x = WrapMonoidal (unwrapApplicative (pure x))
  WrapMonoidal fs <*> WrapMonoidal xs = WrapMonoidal
    (unwrapApplicative (WrapApplicative fs <*> WrapApplicative xs))

instance Applicative m => Monoidal (WrappedMonoidal m) where
  unit = WrapMonoidal (unwrapApplicative unit)
  WrapMonoidal xs >*< WrapMonoidal ys = WrapMonoidal
    (unwrapApplicative (WrapApplicative xs >*< WrapApplicative ys))

newtype WrappedMonad m a = WrapMonad {unwrapMonad :: m a}

instance Covariant m => Functor (WrappedMonad m) where
  fmap f (WrapMonad xs) = WrapMonad (unwrapFunctor (fmap f (WrapFunctor xs)))

instance Functor m => Covariant (WrappedMonad m) where
  WrapMonad xs <&> f = WrapMonad (unwrapFunctor (WrapFunctor xs <&> f))


instance Monoidal m => Applicative (WrappedMonad m) where
  pure x = WrapMonad (unwrapApplicative (pure x))
  WrapMonad fs <*> WrapMonad xs = WrapMonad
    (unwrapApplicative (WrapApplicative fs <*> WrapApplicative xs))

instance Applicative m => Monoidal (WrappedMonad m) where
  unit = WrapMonad (unwrapApplicative unit)
  WrapMonad xs >*< WrapMonad ys = WrapMonad
    (unwrapApplicative (WrapApplicative xs >*< WrapApplicative ys))

instance Triad m => Monad (WrappedMonad m) where
   (WrapMonad ma) >>= fm = WrapMonad $ join $ ma <&> (unwrapMonad . fm)
-- wat is tis?::  ((unwrapMonad .) -> k)
-- join  :: m (m a) -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
-- (<&>) :: m a -> (a -> b) -> m b

instance Monad m => Triad (WrappedMonad m) where
  join (WrapMonad xs) = WrapMonad $  xs >>= (id . unwrapMonad)
-- join  :: m (m a) -> m a
-- (>>=) :: m a -> (a -> m b) -> m b
-- (<&>) :: m a -> (a -> b) -> m b



newtype WrappedTriad m a = WrapTriad {unwrapTriad :: m a}

instance Covariant m => Functor (WrappedTriad m) where
  fmap f (WrapTriad xs) = WrapTriad (unwrapFunctor (fmap f (WrapFunctor xs)))

instance Functor m => Covariant (WrappedTriad m) where
  WrapTriad xs <&> f = WrapTriad (unwrapFunctor (WrapFunctor xs <&> f))

instance Monoidal m => Applicative (WrappedTriad m) where
  pure x = WrapTriad (unwrapApplicative (pure x))
  WrapTriad fs <*> WrapTriad xs = WrapTriad
    (unwrapApplicative (WrapApplicative fs <*> WrapApplicative xs))

instance Applicative m => Monoidal (WrappedTriad m) where
  unit = WrapTriad (unwrapApplicative unit)
  WrapTriad xs >*< WrapTriad ys = WrapTriad
    (unwrapApplicative (WrapApplicative xs >*< WrapApplicative ys))

instance Triad m => Monad (WrappedTriad m) where
  WrapTriad xs >>= ((unwrapTriad .) -> k) = WrapTriad
    (unwrapMonad (WrapMonad xs >>= WrapMonad . k))

instance Monad m => Triad (WrappedTriad m) where
  join ((<&> unwrapTriad) -> WrapTriad xs) = WrapTriad
    (unwrapMonad (join (WrapMonad (fmap WrapMonad xs))))
