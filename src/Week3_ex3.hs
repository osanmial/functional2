{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3_ex3 where
import Control.Applicative

data ParseError = SomethingWentWrong
  deriving Show



{-
let p = Parser 4 \ cs -> case sc of 'w' : cs' -> Right (cs', 42); cs' -> Left OhNo
runParser p "whatever"
runParser p "thatever"
-}


