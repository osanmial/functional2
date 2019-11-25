module Week4.Exercise4 where
import Week4.Exercise3
import Week3.Exercise3
import Control.Applicative
import Data.List
import Debug.Trace
import Data.Either.Combinators

instance Monad Parser where
  (Parser ma) >>= faPmb = Parser (\s -> g s) where
    g s = case ma s of
      Left e -> Left e
      Right (s,a) -> runParse (faPmb a) s

--------------------------------------------------------------------------------
----Grammar parsers-------------------------------------------------------------

pExpr :: Parser Expr
pExpr = pAdd <* removeEmpties <* eof

pAdd :: Parser Expr
pAdd = defaulCombine pMul pAdds Add

pAdds :: Parser (Maybe Expr)
pAdds = optional pAdds'

pAdds' :: Parser Expr
pAdds' = do
  eSingle '+'
  defaulCombine pMul pAdds Add
  
pMul :: Parser Expr
pMul = defaulCombine pOther pMuls Mul

pMuls :: Parser (Maybe Expr)
pMuls = optional pMuls'

-- Parses structure: ('*' other pMuls)
pMuls' :: Parser Expr
pMuls' = do
  eSingle '*'
  defaulCombine pOther pMuls Mul

pOther :: Parser Expr
pOther =  pSub <|> pZero <|> pOne <|> pLet <|> pVar

-- sub : '(' add ')' ;
pSub :: Parser Expr
pSub = do
  eSingle '('
  ex <- pAdd
  eSingle ')'
  pure ex

pLet :: Parser Expr  
pLet = do
  eChunk2 "let"
  var           <- pIdent
  eSingle '='
  expr1         <- pAdd
  eChunk2 "in"
  expr2         <- pAdd
  pure $ Let var expr1 expr2

pVar :: Parser Expr
pVar = do
  out <- pIdent
  pure $ Var out

--------------------------------------------------------------------------------
----Primitives that handle empty spaces-----------------------------------------

eSingle x  = removeEmpties *> single x
eOneOf xs  = removeEmpties *> oneOf xs
eChunk2 xs = removeEmpties *> chunk2 xs

--------------------------------------------------------------------------------
----Primitive 'number' parsers--------------------------------------------------

pZero :: Parser Expr
pZero = do
  eSingle '0'
  pure Zero

pOne :: Parser Expr
pOne = do
  eSingle '1'
  pure One

--------------------------------------------------------------------------------
----Character parsers-----------------------------------------------------------

pSmall :: Parser Char
pSmall = eOneOf ['a'..'z'] -- [a-z] | [_] ;
pLarge :: Parser Char
pLarge = eOneOf ['A'..'Z'] -- [A-Z] ;
pDigit :: Parser Char
pDigit = eOneOf ['0'..'9'] -- [0-9] ;
pPrime :: Parser Char
pPrime = eSingle '\'' -- ['] ;

-- Parser to find if a char is written according to a rule from the set.
oneOfCharParsers :: Parser Char
oneOfCharParsers = foldl1' (<|>) [pSmall, pLarge, pDigit, pPrime]

--------------------------------------------------------------------------------
----String parsers--------------------------------------------------------------

pIdent :: Parser String
pIdent = let manyCharacters = do
               a1 <- pSmall
               a2 <- pIdent'
               pure (a1 : a2)
         in
           manyCharacters <|> (:[]) <$> pSmall

pIdent' :: Parser String
pIdent' = let manyCharacters = do
                a1 <- oneOfCharParsers
                a2 <- pIdent'
                pure (a1 : a2)
          in
            manyCharacters <|> (:[]) <$> oneOfCharParsers 


removeEmpties :: Parser ()
removeEmpties = (\x -> ()) <$> many (oneOf ['\t','\n','\f','\r', ' '])
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
defaulCombine :: Monad m => m a -> m (Maybe a) -> (a -> a -> a) -> m a
defaulCombine fst snd cons = do
  x <- fst
  y <- snd
  case y of
    Nothing -> return x
    Just y  -> return $ cons x y

test str = case (runParse pExpr) str of
             Right (s, v) -> (s, evalDeep v)
             Left _       -> ("", Nothing)

closedString :: String
closedString = "let \
  \ two = 1 + 1 in let \
  \ three = 1 + two in let \
  \ nine = three * three in \
  \ 1 + three * (1 + nine)"


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
  

ma :: IO ()
ma = do
  putStrLn "Im a calculator! Give me calculable!"
  putStrLn "Quit by writing :q"
  logicloop
  return ()


--I cound test micro lenses here.
--test inp = show $ evalDeep . snd <$> (rightToMaybe (runParse (pExpr) inp))

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
