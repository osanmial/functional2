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
          left -> left
        left -> left

eof :: Parser ()
eof = Parser {parse = g} where
  g [] = Right ("", ())
  g _  = Left SomethingWentWrong

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser {parse = g} where
  g []          = Left SomethingWentWrong
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
chunk []     = fmap (\x -> "") eof
chunk (x:xs) = pure (++) <*> g <*> chunk xs
  where
    g = fmap (\x -> [x]) (single x)
