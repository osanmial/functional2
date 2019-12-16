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



breadth :: Expr -> Int 
breadth Zero= 1
breadth One= 1
breadth (Add exp1 exp2)=1 + breadth exp1 + breadth exp2
breadth (Mul exp1 exp2)=1 + breadth exp1 + breadth exp2
breadth (Let s exp1 exp2)=2 + breadth exp1 + breadth exp2 
breadth (Var _)= 1

comp a b = if a>b then
  a + 1
  else
  b + 1

nestingDepth :: Expr -> Int
nestingDepth Zero = 1
nestingDepth One = 1
nestingDepth (Add a b )= comp (nestingDepth a) (nestingDepth b)
nestingDepth (Mul a b )= comp (nestingDepth a) (nestingDepth b)
nestingDepth (Let _ a b) = comp (nestingDepth a) (nestingDepth b) 
nestingDepth (Var _ )= 1


