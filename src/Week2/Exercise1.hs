module Week2.Exercise1 where

import Prelude hiding (Functor, fmap, map)
import Data.Monoid.Endo -- endo
import Data.Functor.Contravariant hiding (Contravariant) -- base
import Data.List.NonEmpty hiding (map) -- containers?
import Data.Map as Map


class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Contravariant m where
  contramap :: (a -> b) -> m b -> m a

class Bifunctor m where
  bimap :: (a -> b) -> (c -> d) -> m a c -> m b d

class Profunctor m where
  dimap :: (a -> b) -> (c -> d) -> m b c -> m a d

--------------------------------------------------------------------------------

-- Bool doesn't admit a functor, because it's not a wrapper of any kind

--------------------------------------------------------------------------------

-- With Nothing the function doesn't matter, the result will always be Nothing
-- 'id x == x so' 'fmap id (Just x)' will always be 'Just x'

--fmap (f.g) (Just x) =? (fmap f . fmap g ) (Just x)
--LHS= fmap (f.g) (Just x)
--{By using M1}
--Just (f.g) (x)
--{By definition of (.)}
-- Just (f(g x))                    --M.2

--RHS= (fmap f . fmap g ) (Just x)
--{By definition of (.)}
--fmap f (fmap g (Just x)) 
--{By definition of M.1}
-- fmap f (Just (g x))
-- {By using M.1}
-- Just (f (g x))                    -- M.3
-- {M.2 and M.3}
-- LHS==RHS

instance Functor Maybe where
  fmap f (Just x) = Just (f x)       --M.1
  fmap _ Nothing  = Nothing

--------------------------------------------------------------------------------

-- Because 'id y = y' then Right (id y) == Right y, so property holds

-- By the nature of fmap we can change the last type, in this example the Right
-- fmap (f.g) (Righ x) =? (fmap f . fmap g ) (Right x)
--LHS=fmap (f.g) (Righ x)
--{By applying E.1}
-- Right((f.g) x)
--{By definition of (.)}
-- Right (f (g x))                    --E.2

--RHS = (fmap f . fmap g ) (Right x)
--By definition of (.)}
-- == fmap f (fmap g (Righ x))
--{By applying E.1}
-- == fmap f (Right (g x))
--{By applying E.1}
-- Right (f (g x))                     -- E.3
-- {From E.2 and E.3} 
-- RHS== LHS

instance Functor (Either a) where
  fmap f (Right y) = Right (f y)      --E.1
  fmap _ (Left x)  = Left x

--------------------------------------------------------------------------------

-- id holds the same way than with regular functor Either

-- bimpa (f . g) (h . i) (Left x) =?   (bimap f h . bimap g i) (Left  x))
-- LHS=  bimap (f . g) (h . i) (Left x)
--{By applying BE.1}
-- Left ((f.g) x)
--{By definition of (.)}
-- Left (f (g x))                             --BE.2

-- RHS = (bimap f h . bimap g i) (Left  x)) 
--{By definition of (.)}
-- bimap f h  (bimap g i (Left x))
--{By applying BE.1}
--bimap f h (Left (g x))
--{By applying BE.1}
--Left (f (g x))                              --BE.3
-- {From BE.3 and BE.2}
-- RHS == LHS 

--In similar way we can prover: 
-- bimpa (f . g) (h . i) (Right x) ==   (bimap f h . bimap g i) (Right  x))
-- This has a value of two types so we can use bifunctor

instance Bifunctor Either where
  bimap f g (Left x)  = Left (f x)                  --BE.1
  bimap f g (Right y) = Right (g y)

--------------------------------------------------------------------------------

-- id property obviously holds because f y = y

-- By the nature of fmap we can change the last type,
-- in this example the latter type of the tuple
-- fmap (f.g) (x,y )=? (fmap f . fmap g ) (x,y)
-- LHS = fmap (f.g) (x,y )
--{By applying BC.1}
--(x, (f.g)  y)
--{By definition of (.)}
--(x, f (g y))                                              --BC.2

--RHS= (fmap f . fmap g ) (x,y)
--{By definition of (.)}
--fmap  f (fmap g (x,y))
--{By applying BC.1}
--fmap f (x , g  y )
--{By applying BC.1}
--- (x , f (g y))                                         --BC.3
-- {From BC.2 and BC.3}
--RHS = LHS

instance Functor ((,) a) where
  fmap f (x, y) = (x, f y)                            --BC.1

--------------------------------------------------------------------------------

-- id property holds the same was as with '(,) a'

--bimap (f . g) (h . i)  (x,y) =?   (bimap f h . bimap g i)  (x,y)
--LHS= bimpa (f . g) (h . i)  (x,y)
--{By applying BI.1}
-- ((f.g) x ,  (h.i) y) 
--{By definition of (.)}
-- (f (g x), h (i y))                                     --BI.2

-- RHS = (bimap f h . bimap g i) (x,y)
--{By definition of (.)}
-- bimap f h (bimap g i (x,y))
--{By applying BI.1}
-- bimap f h  ( g x, i y)
--{By applying BI.1}
-- (f (g x), h (i y))                                   -- BI.3
-- {From BI.2 and BI.3}
-- RHS==LHS

instance Bifunctor (,) where
  bimap f g (x, y) = (f x, g y)                      -- BI.1 

--------------------------------------------------------------------------------

-- Endo a contains function of type a -> a and in fmap we have no function to convert the input value of the function into the end type endo b where the function would be of type b -> b as we only have a function of type (a -> b) in fmap.

-- instance Functor Endo where
--   fmap f (Endo g) = Endo ()

--for contravariant the same applies, but backwards.
--For multi kind functors the problem is that endo has only one kind. 

--------------------------------------------------------------------------------

-- fmap (f.g)  x= ?(fmap f . fmap g ) x
--LHS = fmap (f.g) x 
-- {By applying A.1}
-- = (f.g) . x
--{By definition of (.)}
-- (\y -> f (g (x y)))                             -- A.4

--RHS= -- (fmap f . fmap g )  x
--{By definition of (.)}
-- = fmap f (fmap  g  x)
-- {By applying A.1}
-- fmap f (g . x)
-- {By applying A.1}
-- f . (g . x )
--{By definition of (.)}
-- (\y-> f (g (x y)))                              -- A.3

instance Functor ((->) x) where
   fmap f g = f . g                               -- A.1

--------------------------------------------------------------------------------

--Contravarian wont work as we have no access to the input value of the function.

--instance Contravariant ((->) x) where
--   contramap f g = f . g

--------------------------------------------------------------------------------

-- wont work as we have no function to handle the input value
--instance Bifuncteor (->) where
--  dimap (f . g) (h . i)  x =? (dimap g h . dimap f i) x
-- LHS =   dimap (f . g) (h . i)  x 
-- {By applying DM.1}
-- =  (h.i ) .x. (f.g)
--{By definition of (.)}
-- (\y->  h $  i  $  x $  f  $  g y )       --DM.2

--RHS=(dimap g h . dimap f i) x
--{By definition of (.)}
-- =dimap g h (dimap f i x)
-- {By applying DM.1}
-- =dimap g h (i . x . f)
-- {By applying DM.1}
-- =h. (i . x . f ) .g
--{By definition of (.)}
-- (\y->  h $  i  $  x $  f  $  g y )         --DM.3
-- {From DM.2 and DM.3}
-- RHS==LHS

instance Profunctor (->) where
  dimap f g h = g . h . f                    --DM.1
  
--------------------------------------------------------------------------------

--(contramap f . contramap g) (Op x) =? contramap (g . f) (Op x)
-- LHS= (contramap f . contramap g ) (Op x)
-- {By definition of (.)}
-- =contramap f (contramap g  (Op x))
-- {By applying Co.1}
-- =contramap f ( Op  (x.g))
-- {By applying Co.1}
-- =Op ((x.g).f)
--{(.) Associative}
-- = Op (x.g.f)                                      --Co.2

--RHS=contramap (g . f) (Op x)
-- {By applying Co.1}
-- Op (x. (g.f))
--{(.) Associative}
-- =Op (x.g.f)                                          --Co.3                        
--{From Co.2  and Co.3}
-- RHS==LHS

instance Contravariant (Op x) where
  contramap f (Op g) = Op (g . f)                -- Co.1
-- f :: (a -> b)
-- g :: (b -> x)
-- ~>  (a -> x)

--------------------------------------------------------------------------------

-- Bifunctor doesn't work because it does not have a function for the intput value of Op.

--------------------------------------------------------------------------------

-- Profunctor does not work because f g are backwards in comparison to Op
--instance Profunctor (Op) where

--  dimap f g (Op h) = (f . h . g)
-- f :: (a -> b)
-- g :: (c -> d)
-- h :: m b c
-- out m a b
    
--------------------------------------------------------------------------------

--() is of kind :: * so it can't be mapped over

--------------------------------------------------------------------------------

-- 'id x == x' so the the content of the list doesn't change, so id property holds

--Prove by induction
-- fmap (f.g)  (xs )==fmap f . fmap g ) (xs)             -- Assumption 
-- fmap (f.g)  (x:xs )=?(fmap f . fmap g ) (x:xs)       --- Claim
-- LHS = fmap (f.g)  (x:xs)
-- {By applying L.1}
-- (f.g) x : fmap (f.g) xs                                         -- L.2 

-- RHS= (famp f . fmap g) (x:xs)
-- {By definition of (.)}
-- famp f (fmap g (x:xs))
--{By applying L.1}
-- = fmap f (g x : fmap g xs)
--{By applying L.1}
-- = f (g x): famp f (fmap g xs )
-- {By definition of (.)}
-- = (f.g) x : (fmap f . fmap g ) xs
-- {From the assumption }
-- = (f.g) x : famp (f.g) xs                                       ---L.3
-- {from L.2 and L.3}
-- RHS== LHS

instance Functor [] where
  fmap _ []     = []                                             
  fmap f (x:xs) = f x : fmap f xs                               --L.1

--------------------------------------------------------------------------------

-- List proofs hold for nonempty, because proofs don't rely on the list being empty

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| fmap f xs

--------------------------------------------------------------------------------

-- Void is of wrong kind

--------------------------------------------------------------------------------

-- 'id x == x' so id property holds

instance Functor IO where
  fmap f i = do
    x <- i
    let b = f x
    pure b

--------------------------------------------------------------------------------

-- if id is applied for every value of map, nothing changes is id property holds

-- fmap (f.g)  xs ==( fmap f . fmap g ) (xs)             -- Assumption *
-- fmap (f.g)  (x:xs )=?(fmap f . fmap g ) (x:xs)       --- Claim
-- LHS= fmap (f.g)  (x:xs)
--{By Applying Map.1}
-- map (f.g) (x:xs)                                        -- Map.2

-- RHS = (fmap f . fmap g ) (x:xs)
-- {By definition of (.)}
-- = fmap f (fmap g (x:xs))
--{By Applying Map.1}
-- = fmap f (map g (x:xs))   
--{By Applying Map.1}
-- = map f (map g (x:xs))
-- {By definition of map}
-- =map f (g x: (map  g xs) )
-- {By definition of map}
-- f (g x)  : map f (map g xs)
-- maps are mappable, obviously
-- {By definition of (.)}
-- (f.g) x : (map f . map g)  xs 
-- {From the map low proved in FP1}
-- (f.g) x :  map (f.g) xs
-- {By definition of (.)}
-- map  (f.g) (x:xs)                         -- Map.3
--{From Map.2 and Map.3}
-- LHS == RHS 

instance Functor (Map k)  where
  fmap f xs = map f xs                   ---- Map.1

