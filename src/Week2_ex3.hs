module Week2_ex3 where

import Control.Monad (join)
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Profunctor

newtype WrappedBifunctor m a b = WrapBifunctor {unwrapBifunctor :: m a b}
  deriving Show

newtype WrappedProfunctor m a b = WrapProfunctor {unwrapProfunctor :: m a b}
  deriving Show

newtype Flip m a b = Flip {runFlip :: m b a}
  deriving Show

newtype Join m a = Join {runJoin :: m a a}

-- fmap (\x -> x+1) (WrapBifunctor (Left 4))
-- WrapBifunctor {unwrapBifunctor = Left 4}

-- fmap (\x -> x+1) (WrapBifunctor (Right 4))
-- WrapBifunctor {unwrapBifunctor = Right 5}
instance Bifunctor m => Functor (WrappedBifunctor m a) where
  fmap f (WrapBifunctor x) = WrapBifunctor (bimap id f x)


-- out = unwrapProfunctor $ fmap (+1000) (WrapProfunctor (length))
-- out "kissa"
instance Profunctor m => Functor (WrappedProfunctor m a) where
  fmap f (WrapProfunctor x) = WrapProfunctor (dimap id f x)

-- fmap (\x -> x+1) (Flip (Left 4))
-- Flip {runFlip = Left 5}

-- fmap (\x -> x+1) (Flip (Right 4))
-- Flip {runFlip = Right 4}
instance Bifunctor m => Functor (Flip m a) where
  fmap f (Flip x) = Flip (bimap f id x)

-- out = runFlip $ (contramap (show :: (Int->String)) (Flip head))
-- out 100
instance Profunctor m => Contravariant (Flip m a) where
  contramap f (Flip x) = Flip (dimap f id x)

-- bimap head (\x -> x+1) (Flip (1, "str"))
-- Flip {runFlip = (False,'s')}
instance Bifunctor m => Bifunctor (Flip m) where
  bimap f g (Flip x) = Flip (bimap g f x)

-- runJoin $ fmap (+1) (Join (3, 5))
-- (4,6)
instance Bifunctor m => Functor (Join m) where
  fmap f (Join x) = Join (bimap f f x)
