module Week7.Exercise3 where
import Control.Exception
import Data.Map
import System.IO.Unsafe (unsafePerformIO)
    
data Expr = Add Expr Expr | Zero | Mul Expr Expr | One |
    Let String Expr Expr | Var String
    deriving Show

isSimple :: Expr -> Bool
isSimple exp= case exp of 
    Let _ _ _ -> False
    otherwise-> case exp of 
        Var _ ->False
        otherwise->  True


comp a b = if a>b then
  a + 1
  else
  b + 1

-- nestingDepth :: Expr -> Int
-- nestingDepth Zero = 1
-- nestingDepth One = 1
-- nestingDepth Add a b = comp a b
-- nestingDepth Mul a b = comp a b
-- nestingDepth Let _ a b = comp a b
-- nestingDepth Var _ = 1

assocAdd :: Expr -> Expr
assocAdd (Add (Add a b) z) = assocAdd (Add a (Add b z))
assocAdd (Add a b) = Add (assocAdd a) (assocAdd b)
assocAdd (Mul a b) = Mul (assocAdd a) (assocAdd b)
assocAdd (Let s a b) = Let s (assocAdd a) (assocAdd b)
assocAdd x = x

assocMul :: Expr -> Expr
assocMul (Mul (Mul a b) z) = assocMul (Add a (Add b z))
assocMul (Add a b) = Add (assocMul a) (assocMul b)
assocMul (Mul a b) = Mul (assocMul a) (assocMul b)
assocMul (Let s a b) = Let s (assocMul a) (assocMul b)
assocMul x = x
