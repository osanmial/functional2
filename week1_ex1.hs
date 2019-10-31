{-# LANGUAGE FlexibleInstances #-}
module Week1_1 where

import Prelude hiding (Eq, (==))
import Data.List.NonEmpty 
import Data.Void
import Data.Map as Map

class Eq a where
  (==) :: a -> a -> Bool


instance Eq Bool where
  (==) True True   = True
  (==) False False = True
  (==) _     _     = False
{-

(~):
  Has the corresponding type.

r:
  First two rules go trough all possible states of reflexivity for boolean values.

s:
  In the first two rules there is no distinction between the second and the first element.

t:
  The first and the second part of the conjunction ( 1.(x~y)=1, 2.(y~z)=1 ) can only apply if the arguments are both True or if they are both False. As such the operation has to evaluate to truth among any of the defined elements as they are all the same.

-}

-- Same deal as bool, addition of 'a' doesn't change things significantly

instance Eq a => Eq (Maybe a) where
  (==) (Just x) (Just y) = x == y
  (==) Nothing Nothing   = True
  (==) _       _         = False

{-

(~):
  Has the corresponding type.

r:
  In Maybe there are two possible top level warpper types:
  Nothing: in which case reflexive attribute follows directly from the definition
  (Just _): where the reflexivity applies if the reflexivity applies to the values contained within in the Just.

s:
  There is nothing separating the arguments from each other in relation to their order, outside of the (==) operator, and we assume that it follows symmetric rule.

t:
  In case of Nothing:
    The first and the second part of the conjunction ( 1.(x~y)=1, 2.(y~z)=1 ) can only apply if the arguments are both Nothing. As such the operation has to evaluate to truth among any of the defined elements as they are all the same.
  In case of Just _:
    The rule applies if it applies for other instances of (==) for the value inside the Just. As we can just replace the Just values with the equality of the values contained within. 

-}



-- Similar situation as the previous two
-- but Lefts and Rights can't be tested reliably, because 'a' and 'b' might be different types
-- So 'Left 1 == Right 1' is still False

instance (Eq a, Eq b) => Eq (Either a b) where
  (==) (Left x) (Left y)   = x == y
  (==) (Right x) (Right y) = x == y
  (==) _         _         = False






-- If values inside tuples match, the tuples themselves match

instance (Eq a, Eq b) => Eq (a, b) where
  (==) (x1, x2) (y1, y2) = (x1 == y1) && (x2 == y2)

-- (a -> a) is an identity function, so this is always True
instance Eq a => Eq (a -> a) where
  (==) _ _ = True

{-

(~): The equality funtion is defined with the correct type.
r: Neither of the inputs are related to the output so the rule holds
s: Neither of the inputs are related to the output so the rule holds
t: Neither of the inputs are related to the output so the rule holds

-}
  


-- Same as with tuples, if all values inside two lists match, they are similar
-- Empty list cannot be handled for some reason

instance Eq a => Eq [a] where
  (==) []  []       = True
  (==) (x:xs) (y:ys) = (x == y) && (xs == ys)
  (==) _      _      = False

-- () has only one instance, so it's always True
instance Eq () where
  (==) x y = True

-- Similar to list, but you don't have worry about empty lists.
instance Eq a => Eq (NonEmpty a) where
  (==) (x :| xs) (y :| ys) = x == y && xs == ys

-- Void has no instances so this can be defined, but it can never be called
instance Eq Void where
  (==) a b = True

instance (Ord k, Eq a) => Eq (Map k a) where
  (==) xs ys = isSubmapOfBy (==) xs ys && isSubmapOfBy (==) ys xs  

--tests = (id::Eq (a->a) => (a->a)) == (id::Eq (a->a) => (a->a))



