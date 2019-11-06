module Week3_ex2 where

import Control.Monad (join)
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Profunctor


instance Bifunctor m => Functor (WrappedBifunctor m a) where
  fmap f (WrapBifunctor x) = _

instance Profunctor m => Functor (WrappedProfunctor m a) where
  fmap f (WrapProfunctor x) = _

instance Bifunctor m => Functor (Flip m a) where
  fmap f (Flip x) = _

instance Profunctor m => Contravariant (Flip m a) where
  contramap f (Flip x) = _

instance Bifunctor m => Bifunctor (Flip m) where
  bimap f g (Flip x) = _

instance Bifunctor m => Functor (Join m) where
  fmap f (Join x) = _
