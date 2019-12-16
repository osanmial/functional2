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