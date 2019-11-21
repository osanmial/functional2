module Week4.Exercise4 where
import Week4.Exercise3
import Week3.Exercise3
import Control.Applicative
import Data.List
import Debug.Trace
--grammar parser:

-- parseGrammar :: String -> Expr
-- parseGrammar x = do
--   any

instance Monad Parser where
  (Parser ma) >>= faPmb = Parser (\s -> g s) where
    g s = case ma s of
      Left e -> Left e
      Right (s,a) -> runParse (faPmb a) s

-- lexems:
--   lADD +
--   lMul *
--   lLet let
--   lIn in
--   lEq =
--   lLBr (
--   lRBr )
--   lOne 1
--   lZero 0
--   lIdent words
--   lEoF ""

--end :: Parser 
--end = do 

--pExpr:: Parser Expr
pExpr :: Parser Expr
pExpr = pLeft <|> pLet <|> pSub <|> pVar --pOne <|> pZero

pLeft = (do
  n <- pNum
  c <- pCalc n
  (pure c)) <|> pNum
  -- (pure pMul One) <|> (end)
pNum = pOne <|> pZero

pCalc :: Expr -> Parser Expr
pCalc left = pMul left <|> pAdd left

pAdd left  = do
  removeEmpties
  term          <- single '+'
  removeEmpties
  right         <- pLeft 
  (return $ Add left right)

pMul left = do
  space <- removeEmpties
  term <- single '*'
  space <- removeEmpties
  right <- pOther
  (pure $ Mul left right)

pOther :: Parser Expr
pOther =  pSub <|> pZero <|> pOne <|> pLet <|> pVar
  
-- sub : '(' add ')' ;
pSub :: Parser Expr
pSub = do
  single '('
  ex <- pExpr
  single ')'
  pure ex
  
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

pIdent :: Parser String
pIdent = (do
  a1 <- pSmall
  a2 <- pIdent'
  pure (a1 : a2)) <|> (:[]) <$> pSmall

pIdent' :: Parser String
pIdent' = (do
  a1 <- oneFromLiftedParserList
  a2 <- pIdent'
  pure (a1 ++ a2)) <|> oneFromLiftedParserList --replace ++ with something more effective. It happens backwards here I think.

oneFromLiftedParserList :: Parser [Char]
oneFromLiftedParserList = foldl1' (<|>) $ (fmap (:[])) <$> [pSmall, pLarge, pDigit, pPrime]
  
pSmall :: Parser Char
pSmall = oneOf ['a'..'z'] -- [a-z] | [_] ;
pLarge :: Parser Char
pLarge =oneOf ['A'..'Z'] -- [A-Z] ;
pDigit :: Parser Char
pDigit =oneOf ['0'..'9'] -- [0-9] ;
pPrime :: Parser Char
pPrime =single '\'' -- ['] ;

removeEmpties :: Parser String
removeEmpties = (do
  e1 <- oneOf ['\t','\n','\f','\r', ' ']
  e2 <- removeEmpties
  (pure "")) <|> (:[])<$> (oneOf ['\t','\n','\f','\r']) <|> (pure "") -- have no clue what is a "line tabulation" or how to deal with it: '\u000b'
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

test x = case test' x of
  Right (s,e) -> evalDeep e
  Left _ -> Just (-100)

---ääh tässä on vielä aika bugejah.
test' x = runParse pExpr x

--"let 1+1=two in (let two+two = nelja in (1 + 0 * nelja + 1))"

--TODO let ei toimi. varför? 
--TODO 1*0+1 ==> 0?
--TODO test "let kissa = 1+1 in kissa*0" ==> 2?

