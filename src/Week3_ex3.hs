{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3_ex3 where
import Control.Applicative

newtype Parser a = Parser {parse :: String -> Either ParseError (String, a)}

data ParseError = SomethingWentWrong
  deriving Show

single :: Char -> Parser Char
single x = Parser {parse = f} where
  f (c:cs)
    | c == x    = Right (cs, c)
    | otherwise = Left SomethingWentWrong

