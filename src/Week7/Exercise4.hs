{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns  #-}
{-# OPTIONS_GHC -fwarn-hi-shadowing #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Week7.Exercise4 where

import Control.Arrow
import Data.Function (on)
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Semigroup (Endo (..), stimesMonoid)
import Data.Set (Set (..))
import qualified Data.Set as Set
import Week4.Exercise3
import Week7.Exercise2
import Week7.Exercise3
import Data.Functor.Foldable hiding (cata, Fix, histo, para, futu, ana, apo, hylo)

import Data.Coerce

import Data.Char (intToDigit)
import Data.Foldable (find)
import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.Semigroup (Endo (..), stimesMonoid)
import Data.Set (Set (..))
import qualified Data.Set as Set
--import Week4.Exercise3
--import Week4.Exercise4
-- import Data.Range.Algebra (Algebra)
-- import Data.Fix
-- import Data.Fix

{-
(f a -> a) one step for nonrecursive generator
(Fix f -> a) all the steps
repeats the algebra untill it is done.
comonad reader field guide
-}

-- cata :: Functor f => (f a -> a) -> Fix f -> a

-- F-Algebra
-- type Algebra f a = f a -> a 

-- type  Expr'   = (Fix (ExprF)) 
-- data ExprF r = AddF r r | Zero | MulF r r | One |
--    LetF String r r | VarF String deriving (Functor)
--  $(deriveShow1 ''Expr'')

-- pattern (Fix (Add l r)) = Add l r
-- pattern (Fix Zero) = Zero  
-- pattern (Fix (Mul l r )) = Mul l r
-- pattern (Fix (One)) = One  
-- pattern (Fix (Let s l r)) = Let s l r
-- pattern (Fix (Var s)) = Var s

isSimple :: Expr' -> Bool
isSimple expr = cata isSimpleAlg expr

isSimpleAlg :: Algebra ExprF Bool
isSimpleAlg expr = case expr of
  LetF _ _ _ -> False
  VarF _ -> False
  AddF False _ -> False
  AddF _ False -> False
  AddF _ _ -> True
  MulF False _ -> False
  MulF _ False -> False
  MulF _ _ -> True
  ZeroF -> True
  OneF -> True

{-case exp of
    Let _ _ _ -> False
    otherwise-> case exp of
        Var _ ->False
        otherwise->  True
-}
  
breadth :: Expr' -> Int
breadth expr = cata breadth' expr

breadth' :: Algebra ExprF Int
breadth' ZeroF = 1
breadth' OneF = 1
breadth' (AddF l r) = 1 + l + r
breadth' (MulF l r) = 1 + l + r
breadth' (LetF s l r) = 1 + l + r
breadth' (VarF _) = 1

assocAdd :: Expr' -> Expr'
assocAdd expr = cata assocAdd' expr

assocAdd':: Algebra ExprF Expr'
assocAdd' (AddF (Fix (AddF a b)) z) =Fix $ AddF a $ Fix (AddF b z)
assocAdd' x = Fix $ x

assocMul :: Expr' -> Expr'
assocMul expr= cata assocMul' expr

assocMul' :: Algebra ExprF Expr'
assocMul'(MulF (Fix (MulF a b)) z) = Fix $ MulF a $ Fix  (MulF b z)
assocMul'  x = Fix $ x

depth :: Expr' -> Int
depth expr= cata depth' expr

depth' :: Algebra ExprF Int 
depth' ZeroF = 1
depth' OneF = 1
depth' (AddF a b ) = comp a  b
depth' (MulF a b ) = comp a  b
depth' (LetF _ a b) = comp  a b
depth' (VarF _ ) = 1


unifyAddZero :: Expr' -> Expr'
unifyAddZero = cata unifyAddZero'

unifyAddZero' :: Algebra ExprF Expr'
unifyAddZero' (AddF (Fix ZeroF) x) = x
unifyAddZero' (AddF x (Fix ZeroF)) = x
unifyAddZero' x = coerce x

unifyMulOne (Mul One x) = x
unifyMulOne (Mul x One) = x
unifyMulOne x = x

codistAddMul :: Expr' -> Expr'
codistAddMul = cata codistAddMul'

codistAddMul' :: Algebra ExprF Expr'
codistAddMul' ok@(AddF (Fix (MulF a b)) (Fix (MulF c d)))
  | a == c , b == d = Fix (MulF (Fix (AddF (Fix OneF) (Fix OneF))) (Fix (MulF a b)))
  | a == c = Fix $ MulF a (Fix $ AddF b d)
  | b == d = Fix $ MulF b (Fix $ AddF a c)
  | otherwise = Fix ok
codistAddMul' x = Fix x

-- type family Base t :: * -> *
--para :: (t (t, a) -> a) -> t -> a
--para t = p where p x = t . fmap ((,) <*> p) $ project x

--para :: Functor m => ProductAlgebra m a -> Fix m -> a
--codistAddMul :: Expr' -> Expr'
--codistAddMul'' = para codistAddMul'''

--codistAddMul :: ProductAlgebra ExprF Expr'
--codistAddMul''' = undefined

-- zygo :: Recursive t => (Base t b -> b) -> (Base t (b, a) -> a) -> t -> a 
-- zygo f = gfold (distZygo f)

--codistAddMul'' = zygo codistAddMul''' codistAddMul''''

-- --codistAddMul''' :: (Base t b -> b)
-- codistAddMul''' :: (Base (ExprF Expr') (Either Expr' (Expr',Expr')) -> (Either Expr' (Expr',Expr')))
-- codistAddMul''' (Mul a b) = Right (a, b)
-- codistAddMul''' o = Left o
  
-- --codistAddMul'''' (Base t (b, a) -> a)
-- codistAddMul'''' :: (Base (ExprF Expr') ((Either Expr' Expr'), Expr') -> Expr')
-- codistAddMul'''' (Add (Fix (Right (a,b)), x) (Fix (Right (c,d) , y)))
--   | a == c , b == d = Fix (MulF (Fix (AddF (Fix OneF) (Fix OneF))) (Fix (MulF a b)))
--   | a == c = Fix $ MulF a (Fix $ AddF b d)
--   | b == d = Fix $ MulF b (Fix $ AddF a c)
--   | otherwise = Fix (Add x y)
-- codistAddMul'''' x = Fix $ fmap snd x

commAdd::  Expr' -> Expr' 
commAdd expr= cata commAdd' expr

commAdd':: Algebra ExprF Expr'
--commAdd' (LetF s x y) = case (x,y) of 
--  (z, w) | z > w -> Fix $ LetF s w z
--  (z, w) -> Fix $ LetF s z w
commAdd' (AddF x y) = case ( x,  y) of
   (z, w) | z > w -> Fix $  AddF w z
   (z, w) -> Fix $  AddF z w
commAdd' (MulF x y) = case  (x, y) of
  (z, w) | z > w -> Fix $ MulF w z
  (z, w) -> Fix $ MulF  z w



-- | Free-monadic version of the `(|||)` function
-- from the `Control.Arrow` module of the `base` package.
(||||) :: (a -> b) -> (m (Free' m a) -> b) -> Free' m a -> b
f |||| g = let
  h (Pure' x) = f x
  h (Free' xs) = g xs in
  h
infixr 2 ||||

-- | Cofree-comonadic version of the `(&&&)` function
-- from the `Control.Arrow` module of the `base` package.
(&&&&) :: (a -> b) -> (a -> m (Cofree' m b)) -> a -> Cofree' m b
f &&&& g = let
  h x = Cofree' (f x) (g x) in
  h
infixr 3 &&&&

type Algebra m a = m a -> a

cata :: Functor m => Algebra m a -> Fix m -> a
cata a = let
  c = cata a in
  a . fmap c . unFix

-- | Algebraic version of the `Endo` type
-- from the `Data.Monoid` module of the `base` package.
newtype Embed m = Embed {appEmbed :: Algebra m (Fix m)}

instance Semigroup (Embed m) where
  Embed g <> Embed f = Embed (g . unFix . f)

instance Monoid (Embed m) where
  mempty = Embed Fix

type Coalgebra m a = a -> m a

ana :: Functor m => Coalgebra m a -> a -> Fix m
ana c = let
  a = ana c in
  Fix . fmap a . c

hylo :: Functor m => Algebra m b -> Coalgebra m a -> a -> b
-- Factored version of `hylo a c = cata a . ana c`.
hylo a c = let
  h = hylo a c in
  a . fmap h . c

-- | Coalgebraic version of the `Endo` type
-- from the `Data.Monoid` module of the `base` package.
newtype Project m = Project {appProject :: Coalgebra m (Fix m)}

instance Semigroup (Project m) where
  Project g <> Project f = Project (g . Fix . f)

instance Monoid (Project m) where
  mempty = Project unFix

type ProductAlgebra m a = m (Fix m, a) -> a

para :: Functor m => ProductAlgebra m a -> Fix m -> a
para a = let
  p = id &&& para a in
  a . fmap p . unFix

type SumCoalgebra m a = a -> m (Either (Fix m) a)

apo :: Functor m => SumCoalgebra m a -> a -> Fix m
apo c = let
  a = id ||| apo c in
  Fix . fmap a . c

hypo :: Functor m => ProductAlgebra m b -> SumCoalgebra m a -> a -> b
hypo a c = para a . apo c

type CofreeAlgebra m a = m (Cofree' m a) -> a

histo :: Functor m => CofreeAlgebra m a -> Fix m -> a
histo a = let
  h = histo a &&&& fmap h . unFix in
  a . fmap h . unFix

type FreeCoalgebra m a = a -> m (Free' m a)

futu :: Functor m => FreeCoalgebra m a -> a -> Fix m
futu c = let
  f = futu c |||| Fix . fmap f in
  Fix . fmap f . c

chrono :: Functor m => CofreeAlgebra m b -> FreeCoalgebra m a -> a -> b
chrono a c = histo a . futu c

-- While we could reimplement all the old functions we have for `Expr`
-- to obtain new equivalent functions for `Expr'`,
-- that would be tedious and, in part,
-- vain due to our poor choice of representation (not de Bruijn).
-- Thus, we opt to merely derive an isomorphism and
-- transport the old functions along it.

fixExprCoalg' :: Coalgebra ExprF Expr
fixExprCoalg' = let
  f :: Expr -> ExprF Expr
  f (Add x y) = AddF x y
  f Zero = ZeroF
  f (Mul x y) = MulF x y
  f One = OneF
  f (Let cs x y) = LetF cs x y
  f (Var cs) = VarF cs in
  f

fixExpr' :: Expr -> Expr'
fixExpr' = ana fixExprCoalg'

unFixExprAlg' :: Algebra ExprF Expr
unFixExprAlg' = let
  f :: ExprF Expr -> Expr
  f (AddF x y) = Add x y
  f ZeroF = Zero
  f (MulF x y) = Mul x y
  f OneF = One
  f (LetF cs x y) = Let cs x y
  f (VarF cs) = Var cs in
  f

unFixExpr' :: Expr' -> Expr
unFixExpr' = cata unFixExprAlg'

-- showsPrecExpr' :: Int -> Expr' -> ShowS
-- showsPrecExpr' n = showsPrecExpr n . unFixExpr'

-- showsExpr' :: Expr' -> ShowS
-- showsExpr' = showsPrecExpr' 0

-- showExpr' :: Expr' -> String
-- showExpr' = flip showsExpr' mempty

-- closedDeepBad' :: Expr'
-- closedDeepBad' = fixExpr' closedDeepBad

-- closedDeep' :: Expr'
-- closedDeep' = fixExpr' closedDeep

instance Eq Expr' where
  (==) = on (==) unFixExpr'

instance Ord Expr' where
  compare = on compare unFixExpr'


