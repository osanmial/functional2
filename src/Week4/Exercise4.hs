module Week4.Exercise4 where
import Week4.Exercise3 -- (Expr (..), evalDeep)
import Week3.Exercise3 -- (Parser (..), single, oneOf)
import Control.Applicative
import Data.List
--grammar parser:

-- parseGrammar :: String -> Expr
-- parseGrammar x = do
--   any

instance Monad Parser where
  (Parser ma) >>= faPmb = Parser (\s -> g s) where
    g s = case ma s of
      Left e -> Left e
      Right (s,a) -> runParse (faPmb a) s

pExpr :: Parser Expr
pExpr = pAdd

pAdd :: Parser Expr
pAdd = do
  add <- pAdd
  space <- removeEmpties
  term <- single '+'
  space <- removeEmpties
  mul <- pMul
  (return $ Add add mul) <|> pAdd

pMul :: Parser Expr
pMul = do
  mul <- pMul
  space <- removeEmpties
  term <- single '*'
  space <- removeEmpties
  other <- pOther
  (pure $ Mul mul other) <|> pOther

pOther :: Parser Expr
pOther =  pSub <|> pZero <|> pOne <|> pLet <|> pVar

-- sub : '(' add ')' ;
pSub :: Parser Expr
pSub = do
  single '('
  add <- pAdd
  single ')'
  pure add
  
pZero :: Parser Expr
pZero = do
  single '0'
  pure Zero
pOne :: Parser Expr
pOne = do
  single '1'
  pure One
pLet :: Parser Expr  
pLet = do
  chunk2 "let"
  var <- pIdent
  single '='
  expr1 <- pAdd
  chunk2 "in"
  expr2 <- pAdd
  pure $ Let var expr1 expr2
  
pVar :: Parser Expr
pVar = do
  out <- pIdent
  pure $ Var out

pIdent :: Parser String
pIdent = do
  a1 <- pSmall
  a2 <- pIdent'
  pure (a1 : a2) <|> (:[])<$> pSmall --Yerm wherm germ erm dweeh.

pIdent' :: Parser String
pIdent' = do
  a1 <- pIdent'
  a2 <- pIdent'
  pure (a1 ++ a2) <|> liftedParserList -- TODO replace ++ with those fancy stuff perhaps even maybe?
  -- { ! getText().equals("let") && ! getText().equals("in") }? ;

liftedParserList :: Parser [Char]
liftedParserList = foldl1' (<|>) $ (fmap (:[])) <$> [pSmall, pLarge, pDigit, pPrime] --Herm erm gerum dwereh.
  
pSmall :: Parser Char
pSmall = oneOf ['a'..'z'] -- [a-z] | [_] ;
pLarge :: Parser Char
pLarge =oneOf ['A'..'Z'] -- [A-Z] ;
pDigit :: Parser Char
pDigit =oneOf ['0'..'9'] -- [0-9] ;
pPrime :: Parser Char
pPrime =single '\'' -- ['] ;
removeEmpties :: Parser String
removeEmpties = do
  e1 <- oneOf ['\t','\n','\f','\r']
  e2 <- removeEmpties
  (pure "") <|> (pure "") -- have no clue what is a "line tabulation" or how to deal with it: '\u000b'
  -- [\t\n\u000b\f\r ] + -> skip ;


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
