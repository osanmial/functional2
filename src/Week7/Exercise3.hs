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
  