{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3.Exercise3 where
import Control.Applicative

newtype Parser a = Parser {parse :: String -> Either ParseError (String, a)}

data ParseError = SomethingWentWrong
  deriving Show

instance Functor Parser where
  fmap f (Parser x) = Parser (g . x)
    where
      g (Left _)  = Left SomethingWentWrong
      g (Right (x, y)) = Right (x, f y) 

instance Applicative Parser where
  pure x = Parser (\s -> Right (s, x))
  Parser pf <*> Parser pa = Parser out
    where
      out s = case pf s of
        Right (fStr, f) -> case pa fStr of
          Right (aStr, a) -> Right (aStr, f a)
          Left left -> Left left
        Left left -> Left left


satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser {parse = g} where
  g (c:cs)
    | f c       = Right (cs, c)
    | otherwise = Left SomethingWentWrong

single :: Char -> Parser Char
single x = satisfy (==x)
  
anySingleBut :: Char -> Parser Char
anySingleBut x = satisfy (/=x)

oneOf :: [Char] -> Parser Char
oneOf xs = satisfy (\x -> elem x xs) 
    
noneOf :: [Char] -> Parser Char
noneOf xs = satisfy (\x -> notElem x xs)

chunk :: String -> Parser String
chunk xs = Parser {parse = f} where
  f (cs) = case g xs cs of
             Right ks -> Right (ks, xs)
             Left _   -> Left SomethingWentWrong

  g :: String -> String -> Either ParseError String
  g []     []     = Right ""
  g []     js     = Left SomethingWentWrong
  g hs     []     = Left SomethingWentWrong
  g (h:hs) (j:js) = if h == j
                  then
                    g hs js
                  else
                    Left SomethingWentWrong
