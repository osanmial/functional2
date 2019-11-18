{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Week3.Exercise3 where
import Control.Applicative

import Data.Typeable

{-

Structure: pure (OPERATOR) <*> PARSER <*> PARSER
Combines parsers one after another and operator determines how their a's are combined

Structure: PARSER <|> PARSER
Returns the function result that passes, if both would pass it returns the first one

Structure: PARSER_CONSTRUCTOR <$> PARSER(3)
Creates a parser(1) that has a parser(2) inside it, the parser(2) seems to be based on
the parser(3) and the input of the parser(1)


Structure: some PARSER
Applies parser constantly until if fails, returns a result
If it doesn't succeed a single time returns left

Structure: many PARSER
Same as some, but never returns a left

Structure: optional PARSER
Never returns a left, 'a' is just wrapped into a maybe

Structure: empty
Always returns left

-}

newtype Parser a = Parser {parse :: String -> Either ParseError (String, a)}

data ParseError = SomethingWentWrong
  deriving Show

instance Typeable a => Show (Parser a) where
  show x = show (typeOf x)

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

instance Alternative Parser where
  empty = Parser (\s -> Left SomethingWentWrong)
  Parser f <|> Parser g = Parser (c)
    where
      c s = case f s of
              Left _ -> g s
              v      -> v
      
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
chunk (x:xs) = pure (++) <*> (fmap (\x -> [x]) (single x)) <*> chunk xs

chunk2 :: String -> Parser String
chunk2 []     = Parser (\x -> Right (x, ""))
chunk2 (x:xs) = (\f -> ([f] ++)) <$> single x <*> chunk2 xs
