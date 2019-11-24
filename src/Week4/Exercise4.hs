module Week4.Exercise4Alt where
import Data.List
import Week4.Exercise3
import Week3.Exercise3
import Control.Applicative
import Data.Either.Combinators
instance Monad Parser where
  (Parser ma) >>= faPmb = Parser (\s -> g s) where
    g s = case ma s of
      Left e -> Left e
      Right (s,a) -> runParse (faPmb a) s


-------------------------
----Grammar parsers------


-- Im starting to lose my sanity because of all the removeEmpties.
-- We have to do something about them.
pExpr :: Parser Expr
pExpr = do
    removeEmpties
    out <- pAdd
    removeEmpties
    pure out

-- There is repetition between pAdd and pMul because this mimics the grammar to which this is based.
pAdd :: Parser Expr
pAdd = (do
  left <- pMul
  removeEmpties
  separ <- single '+'
  removeEmpties
  right <- pAdd
  pure (Add left right)) <|> pMul

-- There is repetition between pAdd and pMul because this mimics the grammar to which this is based.  
pMul :: Parser Expr
pMul =  (do
  left <- pOther
  removeEmpties
  separ <- single '*'
  removeEmpties
  right <- pMul
  pure (Mul left right)) <|> pOther

pOther = pSub <|> pZero <|> pOne <|> pLet <|> pVar

pSub = do
  single '('
  removeEmpties
  expr <- pExpr
  removeEmpties
  single ')'
  pure expr

pZero = do
  single '0'
  pure (Zero)

pOne = do
  single '1'
  pure (One)


pLet :: Parser Expr  
pLet = do
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
  pure $ Let var expr1 expr2

pVar :: Parser Expr
pVar = do
  out <- pIdent
  pure $ Var out


-------------------------
----Character parsers----

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

-----------------------
----String parsers-----

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


logicloop = do
  inp <- getLine
  let continue = case (inp) of
        ':':'q':ys -> pure ()
        _ -> do
          let out = (runParse (pExpr) inp)
          putStrLn $show out
          putStrLn $ show $ evalDeep . snd <$> (rightToMaybe out)
          logicloop
  continue

main :: IO ()
main = do
  putStrLn "Im a calculator! give me calculable!"
  putStrLn "Quit by writing :q"
  logicloop
  return ()

--I cound test micro lenses here.
test inp = show $ evalDeep . snd <$> (rightToMaybe (runParse (pExpr) inp))

-- Why have tuples no classes in hoogle?
tests = (test <$> )<$>
  [( "27", "let a = 1+1+1+1+1 in 1+a*a+1")
  ,( "0", "let a = 1+1+1+1+1 in a*a*0")
  ,( "0", "let a = 1+1+1+1+1 in 0*a*1")
  ,( "50", "let a = 1+1+1+1+1 in a*a+a*a")
  ,( "250", "let a = 1+1+1+1+1 in a*(a+a)*a")
  ,( "5", "let a = 1+1+1+1+1 in let b=1+1 in let a = b + 1 in a+b")
  ,( "-> Nothing", "let a = 1+1+1+1+1 in let b=1+1 in let a = b + 1 in a+b+c") --shoul fail
  ,( "-> Nothing", "let a = 1+1 in 1+1+1+1+1+1+") --should fail
  ,( "-> Nothing", "1 (1+1") --should fail? but does currently not.
  ,( "-> Nothing", "1 1") --should fail? but does currently not.
  ,( "-> Nothing", "-1") --should fail, "-> Nothing"
  ,( "-> Nothing", "+0")--should fail
  ,( "-> Nothing", "1kissa") --should fail? but does not.
  ,( "-> Nothing", "kissa")] --should fail
  --Errors don't rise up from the end. It just stops parsing.

 
