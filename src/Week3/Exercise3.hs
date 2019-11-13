{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3.Exercise3 where
import Control.Applicative

newtype Parser a = Parser {parse :: String -> Either ParseError (String, a)}

data ParseError = SomethingWentWrong
  deriving Show

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser {parse = g} where
  g (c:cs)
    | f c       = Right (cs, c)
    | otherwise = Left SomethingWentWrong

single :: Char -> Parser Char
single x = Parser {parse = f} where
  f (c:cs)
    | c == x    = Right (cs, c)
    | otherwise = Left SomethingWentWrong

anySingleBut :: Char -> Parser Char
anySingleBut x = Parser {parse = f} where
  f (c:cs)
    | c /= x    = Right (cs, c)
    | otherwise = Left SomethingWentWrong

oneOf :: [Char] -> Parser Char
oneOf xs = Parser {parse = f} where
  f (c:cs)
    | elem c xs = Right (cs, c)
    | otherwise = Left SomethingWentWrong
    
noneOf :: [Char] -> Parser Char
noneOf xs = Parser {parse = f} where
  f (c:cs)
    | notElem c xs = Right (cs, c)
    | otherwise = Left SomethingWentWrong

chunck :: String -> Parser String
chunck (xs) = Parser {parse = f} where
  f (cs) = case g xs cs of
             Right ks -> Right (ks, xs)
             Left _   -> Left SomethingWentWrong

  g :: String -> String -> Either ParseError String
  g []     js     = Right js
  g hs     []     = Left SomethingWentWrong
  g (h:hs) (j:js) = if h == j
                  then
                    case g hs js of
                      Right s -> Right s
                      Left _  -> Left SomethingWentWrong
                  else
                    Left SomethingWentWrong
           
