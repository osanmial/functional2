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


-- data Expr = Add Expr Expr | Zero | Mul Expr Expr | One |
--   Let String Expr Expr | Var String


commAdd::  Expr -> Expr
commAdd (Let s exp1 exp2)= (Let s (commAdd exp1) (commAdd exp2))
commAdd (Add exp1 exp2)
  | exp1 > exp2 = Add exp2 exp1 
  |otherwise= Add exp1 exp2

closedStringBad :: String
closedStringBad = "let \
   \ two = 1 + 1 in let \
   \ three = 1 * (1 + two * 1) in let \
   \ five = let \
     \ four = two * two in \
     \ 1 + four in let \
   \ six = let \
     \ seven = 1 + six in \
     \ two * three in let \
   \ eight = two * four in let \
   \ nine = 0 + three * (three + 0) in \
   \ (0 * five + 1) + (three * (1 + nine) + eight * 0)"
  
--closedDeepBad :: Expr
--closedDeepBad = case parseExpr closedStringBad of
--  Left e -> error (show e)
--  Right x -> x

-- | An improved version of the `showIntAtBase` function
-- from the `Numeric` module of the `base` package.
showIntAtBase' :: forall a. Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase' n f = let
  g :: a -> ShowS
  g p = case p `divMod` n of
    (0, 0) -> id
    (0, m) -> showChar (f (fromEnum m))
    (d, m) -> g d . showChar (f (fromEnum m)) in
  g

-- | A dual of the `keysSet` function
-- from the `Data.Map` module of the `containers` package.
elemsSet :: Ord a => Map k a -> Set a
elemsSet xs = Set.fromList (Map.elems xs)

-- | A copy of the `thenCmp` function
-- from the `Language.Haskell.TH.Syntax` module
-- of the `template-haskell` package.
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ y = y
thenCmp x _ = x

namePrefixes :: [String]
namePrefixes = fmap pure ['x' .. 'z']

freshName :: Set String -> String
freshName css = case find (`Set.notMember` css)
  [(showString ds . showIntAtBase' 10 intToDigit n) mempty |
    n <- [0 :: Int ..], ds <- namePrefixes] of
  -- There are `on (*) toInteger maxBound (length namePrefixes)`
  -- possible names and they are generated in a sorted order
  -- to keep the behavior of the renamer predictable.
  Nothing -> error "Out of names"
  Just cs -> cs

findFree :: Expr -> Set String
findFree = let
  f :: Expr -> Set String
  f (Add x y) = f x <> f y
  f Zero = mempty
  f (Mul x y) = f x <> f y
  f One = mempty
  f (Let cs x y) = f x <> Set.delete cs (f y)
  f (Var cs) = Set.singleton cs in
  f

compareExpr :: Expr -> Expr -> Ordering
compareExpr = let
  f :: Map String String -> Map String String -> Expr -> Expr -> Ordering
  f css dss (Add x y) (Add z w) = f css dss x z `thenCmp` f css dss y w
  f _ _ Zero Zero = EQ
  f css dss (Mul x y) (Mul z w) = f css dss x z `thenCmp` f css dss y w
  f _ _ One One = EQ
  f css dss (Let cs x y) (Let ds z w) = f css dss x z `thenCmp` let
    es = freshName (findFree y <> findFree w) in
    f (Map.insert cs es css) (Map.insert ds es dss) y w
  f css dss (Var cs) (Var ds) = case (Map.lookup cs css, Map.lookup ds dss) of
    (Nothing, Nothing) -> cs `compare` ds
    -- We must not compare free variables to bound variables,
    -- because their ordering may be changed by the renamer.
    -- This is fine, because free variables are never renamed.
    (Nothing, Just _) -> LT
    (Just _, Nothing) -> GT
    (Just es, Just fs) -> es `compare` fs
  -- The remaining cases are chosen in such a way that
  -- "more constant" terms come before "more varying" ones.
  -- This kind of convention may seem arbitrary,
  -- but it comes up frequently in functional programming.
  f _ _ Add {} Zero {} = GT
  f _ _ Add {} Mul {} = LT
  f _ _ Add {} One {} = GT
  f _ _ Add {} Let {} = LT
  f _ _ Add {} Var {} = LT
  f _ _ Zero {} Add {} = LT
  f _ _ Zero {} Mul {} = LT
  f _ _ Zero {} One {} = LT
  f _ _ Zero {} Let {} = LT
  f _ _ Zero {} Var {} = LT
  f _ _ Mul {} Add {} = GT
  f _ _ Mul {} Zero {} = GT
  f _ _ Mul {} One {} = GT
  f _ _ Mul {} Let {} = LT
  f _ _ Mul {} Var {} = LT
  f _ _ One {} Add {} = LT
  f _ _ One {} Zero {} = GT
  f _ _ One {} Mul {} = LT
  f _ _ One {} Let {} = LT
  f _ _ One {} Var {} = LT
  f _ _ Let {} Add {} = GT
  f _ _ Let {} Zero {} = GT
  f _ _ Let {} Mul {} = GT
  f _ _ Let {} One {} = GT
  f _ _ Let {} Var {} = LT
  f _ _ Var {} Add {} = GT
  f _ _ Var {} Zero {} = GT
  f _ _ Var {} Mul {} = GT
  f _ _ Var {} One {} = GT
  f _ _ Var {} Let {} = GT in
  f mempty mempty

instance Eq Expr where
  x == y = compareExpr x y == EQ

instance Ord Expr where
  compare = compareExpr