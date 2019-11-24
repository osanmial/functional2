module Week4.Exercise4 where
import Week4.Exercise3
import Week3.Exercise3
import Control.Applicative
import Data.List
import Debug.Trace

instance Monad Parser where
  (Parser ma) >>= faPmb = Parser (\s -> g s) where
    g s = case ma s of
      Left e -> Left e
      Right (s,a) -> runParse (faPmb a) s


pExpr :: Parser Expr
pExpr = pAdd


-- lexems:
--   Add +
--   Mul *
--   Let let
--   In in
--   Eq =
--   LBr (
--   RBr )
--   One 1
--   Zero 0
--   Ident words
--   empty symbols
--   EoF ""

--------------------------------------------------------------------------------
----Primitive 'number' parsers--------------------------------------------------

pZero :: Parser Expr
pZero = do
  single '0'
  pure Zero

pOne :: Parser Expr
pOne = do
  single '1'
  pure One

--------------------------------------------------------------------------------
----Character parsers-----------------------------------------------------------

pSmall :: Parser Char
pSmall = oneOf ['a'..'z'] -- [a-z] | [_] ;
pLarge :: Parser Char
pLarge = oneOf ['A'..'Z'] -- [A-Z] ;
pDigit :: Parser Char
pDigit = oneOf ['0'..'9'] -- [0-9] ;
pPrime :: Parser Char
pPrime = single '\'' -- ['] ;

-- Parser to find if a char is written according to a rule from the set.
oneOfCharParsers :: Parser Char
oneOfCharParsers = foldl1' (<|>) [pSmall, pLarge, pDigit, pPrime]

--------------------------------------------------------------------------------
----String parsers-----------------------------------------------------------

pIdent :: Parser String
pIdent = (do
  a1 <- pSmall
  a2 <- pIdent'
  pure (a1 : a2)) <|> (:[]) <$> pSmall

pIdent' :: Parser String
pIdent' = (do
  a1 <- oneOfCharParsers
  a2 <- pIdent'
  pure (a1 : a2)) <|> (:[]) <$> oneOfCharParsers 


removeEmpties :: Parser String
removeEmpties = (do
  e1 <- oneOf ['\t','\n','\f','\r', ' ']
  e2 <- removeEmpties
  (pure "")) <|> (:[]) <$> (oneOf ['\t','\n','\f','\r']) <|> (pure "")
  -- have no clue what is a "line tabulation" or how to deal with it: '\u000b'
  -- [\t\n\u000b\f\r ] + -> skip ;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Primitive instance for things that takes
-- first value: Monad with plain value inside it
-- second value: Monad with a maybe inside it
-- aconstructor
-- returns the plain value if second value is nothing
-- if second is Just, it returns the both values combined with the constructor
--pPrim :: Monad m => m a -> m (Maybe a) -> (a -> a -> a) -> m a
pPrim :: Parser a -> Parser (Maybe a) -> (a -> a -> a) -> Parser a
pPrim fst snd cons = do
  removeEmpties
  x <- fst
  removeEmpties
  y <- snd
  removeEmpties
  case y of
    Nothing -> return x
    Just y  -> return $ cons x y


pAdd :: Parser Expr
pAdd = pPrim pMul pAdds Add

pAdds :: Parser (Maybe Expr)
pAdds = optional pAdds'

pAdds' :: Parser Expr
pAdds' = do
  single '+'
  pPrim pMul pAdds Add

  
pMul :: Parser Expr
pMul = pPrim pOther pMuls Mul

pMuls :: Parser (Maybe Expr)
pMuls = optional pMuls'

-- Parses structure: ('*' other pMuls)
pMuls' :: Parser Expr
pMuls' = do
  single '*'
  pPrim pOther pMuls Mul
  

pOther :: Parser Expr
pOther =  pSub <|> pZero <|> pOne <|> pLet <|> pVar


-- sub : '(' add ')' ;
pSub :: Parser Expr
pSub = do
  single '('
  ex <- pExpr
  single ')'
  pure ex
 

pLet :: Parser Expr  
pLet = do
  removeEmpties
  chunk2 "let"
  removeEmpties
  var           <- pIdent
  removeEmpties
  single '='
  removeEmpties
  expr1         <- pExpr
  removeEmpties
  chunk2 "in"
  removeEmpties
  expr2         <- pExpr
  removeEmpties
  pure $ Let var expr1 expr2

pVar :: Parser Expr
pVar = do
  out <- pIdent
  pure $ Var out





{-

grammar ExprLR ;

expr : add ;
add : add '+' mul | mul ;
mul : mul '*' other | other ;
other : sub | zero | one | let | var ;
sub : '(' add ')' ;
zero : '0' ;
one : '1' ;
let : 'let' Ident '=' add 'in' add ;
var : Ident ;

Ident : Small ( Small | Large | Digit | Prime ) *
  { ! getText().equals("let") && ! getText().equals("in") }? ;
Small : [a-z] | [_] ;
Large : [A-Z] ;
Digit : [0-9] ;
Prime : ['] ;
Space : [\t\n\u000b\f\r ] + -> skip ;

-}      

test str = case (runParse pExpr) str of
             Right (s, v) -> (s, evalDeep v)
             Left _       -> ("", Nothing)

--Failure: test "let x = 1+1 in (2*2)*(x+x)"
