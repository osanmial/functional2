module Week4.Exercise3 where
import Control.Exception
import Data.Map
import System.IO.Unsafe (unsafePerformIO)

data Expr = Add Expr Expr | Zero | Mul Expr Expr | One |
  Let String Expr Expr | Var String
  deriving (Show, Eq)

join :: Monad m => m (m a) -> m a
join mma = mma >>= id
{-
  mma
  >>= :: m ma -> (ma-> m b) -> mb
  mb
-}

closedDeep =
  Let "two" (Add One One) $
  Let "three" (Add One (Var "two")) $
  Let "nine" (Mul (Var "three") (Var "three")) $
  Add One (Mul (Var "three") (Add One (Var "nine")))

-- I didin't manage to make this work with just a plain undefined without capturing it in a catch, but if we consider that it is supposed to represent a value without a definition then this should be comparable.
openDeep = Add One (Var "undefined")

evalDeep inp = (evalDeep'' (inp, empty))
  -- in
  --   unsafePerformIO (catch (seq x (pure (pure (Just x))))
  --                    (\ e -> seq (e :: SomeException) (pure Nothing)))


evalDeep' :: (Expr, Map String Int) -> Int
evalDeep' (x,vars) = case x of
  One -> 1
  Zero -> 0
  Var var -> vars ! var
  Add a b -> evalDeep' (a,vars) + evalDeep' (b,vars) 
  Mul a b -> evalDeep' (a,vars) * evalDeep' (b,vars)
  Let name a b -> evalDeep' (b , insert name (evalDeep' (a,vars)) vars)


-- a version with Maybe
evalDeep'' :: (Expr, Map String (Maybe Int)) -> Maybe Int
evalDeep'' (x,vars) = case x of
  One -> Just 1
  Zero -> Just 0
  Var str -> join $ vars !? str
  Add a b -> (+) <$> evalDeep'' (a,vars) <*> evalDeep'' (b,vars) 
  Mul a b -> (*) <$> evalDeep'' (a,vars) <*> evalDeep'' (b,vars)
  Let name a b -> evalDeep'' (b , insert name (evalDeep'' (a,vars)) vars)





