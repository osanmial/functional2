{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3_ex3 where
import Control.Applicative

data ParseError = SomethingWentWrong
  deriving Show