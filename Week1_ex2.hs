{-# Language FlexibleInstances#-}
module Week1_ex2 where

import Prelude hiding (Semigroup, Monoid, (<>), mempty)
import Data.Either
import Data.List.NonEmpty
import Data.Map as Map

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a


-- AND operator is accociative
-- a <> (b <> c)= (a <> b) <> c 
-- LHS = a <> (b <> c)
-- a && (b && c) = (a && b) && c 
-- { By bool.a }
-- a <> (b && c) 
-- { By bool.a }
-- a && (b && c)
-- {And is assoctive}
-- (a && b) && C
--{ By bool.a  in reverse}
-- (a <> b ) && c 
--{ By bool.a  in reverse}
-- (a <> b ) <> c 
-- = RHS
instance Semigroup Bool where
  a <> b = a && b      --- boool.1

-- True is mempty in terms of AND operation
instance Monoid Bool where
  mempty = True



-- Just xs <> (Just ys <> Just zs) =? (Just xs <> Just ys) <> Just zs 
--LHS=	Just xs <> (Just ys <> Just zs)
--{By applying Just.1}
-- =Just xs <> Just (xs <> ys )
--{By applying Just.1}
-- = Just (xs <> ys <> zs)  ... .2
--RHS=  (Just xs <> Just ys) <> Just zs 
--{By applying Just.1}
-- = Just (xs <> ys) <> Just zs
--{By applying Just.1}
-- = Just (xs <> ys <> zs)  .....1
-- {from .1 and .2 }
--RHS == LHS
instance Semigroup a => Semigroup (Maybe a) where
  (Just xs) <> (Just ys) = Just (xs <> ys)   --Just.1
  Nothing  <> ys         = ys
  xs       <> Nothing    = xs

instance Monoid a => Monoid (Maybe a) where
  mempty = Nothing

--(Left x) <> ((Left y) <> (Left z))=?((Left x) <> (Left y))<> (Left z)
--LHS =(Left x) <> ((Left y) <> (Left z))
--{By using Either.1}
-- (Left x ) <> (Left (y <> z ))
--{By using Either.1}
--Left (x <> (y <> z))
--{x,y,z belong to Semigroup} 
-- = Left ((x <> y ) <> z )
-- {By using Either.1 in reverse }
-- = (Left (x <> y)) <> (Left z)
-- {BY using Either.1 in reverse}
-- = ((Left x) <> (Left y)) <> (Left z)
-- == RHS 
instance (Semigroup a, Semigroup b) => Semigroup (Either a b) where
  (Left x)  <> (Left y)  = Left  (x <> y)       --- Either.1
  (Right x) <> (Right y) = Right (x <> y)
  (Left x)  <> _         = Left x
  _         <> Left y    = Left y

instance (Monoid a, Monoid b) => Monoid (Either a b) where
  mempty = Right mempty



--Proving associativity for id function
-- a <> (b <> c)= (a <> b) <> c 
-- LHS=a <> (b <> c)
--{By applying fun.1 } 
-- = a <> (id)
--{By applying fun.1 } 
-- = id                      ...fun.2 

-- RHS=(a <> b) <> c
--{By applying fun.1 } 
-- = (id) <> c 
--{By applying fun.1 } 
-- = id                     ...fun.3
--{from fun.2 and fun.3}
-- RHS == LHS
instance Semigroup (a -> a) where
  _ <> _ = id       --- fun.1

instance Monoid (a -> a) where
  mempty = id

--Proving associativity for Pair. 
-- (x1, y1) <> ((x2, y2) <> (x3,y3)) =? ((x1, y1) <> (x2, y2)) <> (x3,y3) 
--LHS= (x1, y1) <> ((x2, y2) <> (x3,y3))
-- {BY applying pa.1}
-- =(x1,y1) <> ((x2 <> x3), (y2 <> y3))
-- {BY applying pa.1}
-- =((x1 <> (x2 <> x3)), (y1 <> (y2 <> y3))
-- {BY depend on that x and y are simigroup so associativity is satisfied}
-- = (((x1<> x2 )<> x3),(y1 <> y2 ) <> y3 )    
-- {By applying -- pa.1 in reverse}
-- ((x1 <> x2),( y1 <> y2)) <> (x3, y3)
-- {By applying -- pa.1 in reverse}
-- ((x1,y1) <> (x2 ,y2)) <> (x3 , y3 )
-- == RHS 
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
 (x1, y1) <> (x2, y2) = ((x1 <> x2), (y1 <> y2))   -- pa.1
instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)



--Proving associativity for (). 
-- () <> (() <> ()) =? (() <> ()) <> () 
--LHS= () <> (() <> ())
-- {BY applying paran.1}
-- () <> (())
-- {BY applying paran.1}
-- = ()

--RHS = (() <> ()) <> ()
-- {BY applying paran.1}
-- = (()) <> ()
-- {BY applying paran.1}
-- = () == LHS  
instance Semigroup () where
  _ <> _ = ()             -- paran.1 
instance Monoid () where
  mempty = ()

-- (xs <> ys )<> zs =? xs <> (ys <> zs )
-- LHS=(xs <> ys )<> zs 
--{By usig str.1} 
--(xs++ys) <> zs
--{By usig str.1} 
--(xs++ys++zs).. str.2
-- RHS= xs <> (ys <> zs )
--{By usig str.1} 
--xs <> (ys++zs)
--{By usig str.1} 
-- (xs++ys++zs)  .. str.3
--{From str.2 and str.3}
-- RHS ==LHS 

instance Semigroup [a] where
  xs <> ys = xs ++ ys   -- str.1 

instance Monoid [a] where
  mempty = []


-- ((x :| xs) <> (y :| ys)) (z:zs) =? (x :| xs) <> ((y :| ys) (z:zs))
-- LHS = ((x :| xs) <> (y :| ys)) (z :|zs) 
-- {By using str*}
-- x:| (xs ++y:ys ) <> ( z:|zs)
-- {By using str*}
-- x:| (xs ++ y:ys ++ z:zs) ... str*1 
-- RHS=(x :| xs) <> ((y :| ys) (z:zs))
-- {By using str*}
-- (x:|xs) <> (y:|(ys++z:zs))
-- {By using str*}
-- x:|(xs ++ y:ys++z:zs)


instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ y:ys) -- str* 


instance (Ord k, Semigroup a) => Semigroup (Map k a) where
  xs <> ys = unionWith (<>) xs ys

instance (Ord k, Monoid a) => Monoid (Map k a) where
  mempty = empty
