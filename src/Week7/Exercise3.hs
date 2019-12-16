{-# LANGUAGE ScopedTypeVariables #-}

module Week7.Exercise3 where

import Data.Char (intToDigit)
import Data.Foldable (find)
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Semigroup (Endo (..), stimesMonoid)
import Data.Set (Set (..))
import qualified Data.Set as Set
import Week4.Exercise3
import Week4.Exercise4


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
breadth (Let s exp1 exp2)=1 + breadth exp1 + breadth exp2 
breadth (Var _)= 1

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

comp a b = if a>b then
  a + 1
  else
  b + 1

depth :: Expr -> Int
depth Zero = 1
depth One = 1
depth (Add a b )= comp (depth a) (depth b)
depth (Mul a b )= comp (depth a) (depth b)
depth (Let _ a b) = comp (depth a) (depth b) 
depth (Var _ )= 1

commAdd::  Expr -> Expr
commAdd exp=undefined

