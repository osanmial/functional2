{-# LANGUAGE EmptyCase #-}

module Week6.Exercise3 where
import Data.Void


-- 1) equivalence is reflexive,
-- r(∼)
-- ∀x:A x≅x

eqReflexiveR :: a -> a
eqReflexiveR = id

eaReflexiveL :: a -> a
eaReflexiveL = id


-----------------------------------------------------------------------------------
-- 2) equivalence is symmetric,
-- s(∼):
-- ∀xy:A x~y → y~x

eqSymmetry :: y -> y
eqSymmetry = id

--eqSymmetry :: (a->b->Bool) -> (b->a->Bool)
--eqSymmetry = flip -- ? or just id?
-- very quite unsure about this one


-----------------------------------------------------------------------------------
-- 3) equivalence is transitive,
-- (∼):∀x,y,z:A
-- x∼y → y∼z → x∼z,

eqTransitive :: a -> a
eqTransitive = id -- ? or and?

--eqTransitive :: Bool -> Bool -> Bool
--eqTransitive a b = and [a,b]
-- Im not at all sure that this is what we are after at all...


-----------------------------------------------------------------------------------
-- 4) addition is proper,
-- p(∼),(+):
-- ∀x,y,z,w:A  x∼y  →  z∼w  →  x+z ∼ y+w,

properAdd :: Either a b -> Either a b
properAdd = id -- ?


-----------------------------------------------------------------------------------
-- 5) addition is associative,
-- a(∼),(+):
-- ∀x,y,z:A x+(y+z) ≅ (x+y)+z

assocAddL :: Either x (Either y z) -> Either (Either x y) z
assocAddL (Left x) = Left $ Left x
assocAddL (Right (Left y)) = Left $ Right y
assocAddL (Right (Right z)) = Right z

assocAddR :: Either (Either x y) z -> Either x (Either y z)
assocAddR (Left (Left x)) = Left x
assocAddR (Left (Right y)) = Right $ Left y
assocAddR (Right z) = Right $ Right z
--Erm... What does this mean? Or does this mean anything?


-----------------------------------------------------------------------------------
-- 6)addition is commutative,
-- c(∼),(+):
-- ∀x,y:A x+y∼y+x

commAddL :: Either x y -> Either y x
commAddL (Left x) = Right x
commAddL (Right x) = Left x


-----------------------------------------------------------------------------------
-- 7) a proof that zero is unitary with respect to addition on the left,
-- ∀x:A 0+x≅x,
{-
funext: ∀A,B:U ∀f,g:A→B (∀x:A f(x)=g(x))→f=g
(≅):U×U→U
A≅B ≡ ∃f:A→B ∃g:B→A g∘f=id∧f∘g=id
definition of isomorphism:  ∃f:A→B ∃g:B→A g∘f=id∧f∘g=id
in this case:  ∃f: Void+A->A  ∃g: A->Void+A g∘f=id∧f∘g=id

witnesses?:
    f : Void+A -> A
  f(m) ≡ Void + z ↦ m (z) 1.composition
    g : A -> Void + A
  g(m) ≡ z ↦ m (Void + z) 2.composition

funext applied twice:
  ∀m:1×C→A ∀(x):C  (g∘f)(m)(x) = id(m)(x)
m and x are arbitrary inhabitants of ther definition so we can work with following:

-- We probably would require lambda calculus for this to be unambiguous?

(g∘f)(m)(x) = id(m)(x)
(g(f(m)))(x) = m(x) 
(z ↦ f(m) (Void + z))(x) = m(x) 2.composition
f(m)(Void + x)= m(x)
((Void + z) ↦ m (z))(Void + x) = m(x) 1.composition
m(x) = m(x)

-- works the other way around as well (f∘g), but I have been too lazy to prove that yet.

-}

leftUnitaryAddL :: Either Void x -> x
leftUnitaryAddL (Right x) = x
leftUnitaryAddL x = case x of {}

leftUnitaryAddR :: x -> Either Void x
leftUnitaryAddR x = Right x


-----------------------------------------------------------------------------------
--8) zero is unitary with respect to addition on the right,
-- ∀x:A x+0≅x

rightUnitaryAddL :: Either x Void -> x
rightUnitaryAddL (Left x) = x
rightUnitaryAddL x = case x of {}

rightUnitaryAddR :: x -> Either x Void
rightUnitaryAddR x = Left x


-----------------------------------------------------------------------------------
--9) multiplication is proper,
-- p(∼),(×):
-- ∀x,y,z,w:A x∼y → z∼w → x×z∼y×w,

-- x≅z, y≅w -- z and w are replaced by x and y as they are related by id morphism?
properMulL :: (x,y) -> (x,y)
properMulL = id

properMulR :: (x,y) -> (x,y)
properMulR = id

-- all morphisms explicitly written out.
-- properMul (a,b) -> (id a, id b)


-----------------------------------------------------------------------------------
--10) multiplication is associative,
-- a(∼),(×):∀x,y,z:A
-- x×(y×z)∼(x×y)×z

assocMulL :: (x,(y,z)) -> ((x,y),z)
assocMulL (x,(y,z)) = ((x,y),z)

assocMulR :: ((x,y),z) -> (x,(y,z)) 
assocMulR ((x,y),z) = (x,(y,z)) 


-----------------------------------------------------------------------------------
--11) multiplication is commutative,
-- c(∼),(×):
-- ∀x,y:A x×y∼y×x,

commMulLR :: (x,y) -> (y,x)
commMulLR (x,y)= (y,x)


-----------------------------------------------------------------------------------
--12) a proof that one is unitary with respect to multiplication on the left,
-- -- l(∼),(×),1:
-- ∀x:A 1×x ≅ x,
{-
funext: ∀A,B:U ∀f,g:A→B (∀x:A f(x)=g(x))→f=g

(≅):U×U→U
A≅B ≡ ∃f:A→B ∃g:B→A g∘f=id∧f∘g=id

definition of isomorphism:  ∃f:A→B ∃g:B→A g∘f=id∧f∘g=id
in this case:  ∃f: ((),A)->A  ∃g: A->((),A) g∘f=id∧f∘g=id

witnesses?:
    f : ((), A) -> A
  f(m) ≡ ((),z) ↦ m (z) 1.composition
    g : A -> ((), A)
  g(m) ≡ z ↦ m ((),z) 2.composition

funext applied twice:
  ∀m:1×C→A ∀(x):C  (g∘f)(m)(x) = id(m)(x)
m and x are arbitrary inhabitants of ther definition so we can work with following:

-- We probably would require lambda calculus for this to be unambiguous?
(g∘f)(m)(x) = id(m)(x)
(g(f(m)))(x) = m(x) 
(z ↦ f(m) ((),z))(x) = m(x) 2.composition
f(m) ((),x) = m(x)
(((),z) ↦ m (z))((),x) = m(x) 1.composition
m(x) = m(x)

-- works the other way around as well (f∘g), but I have been too lazy to prove that yet.

-}

leftUnitaryMulL :: ((), a) -> a
leftUnitaryMulL ((),a) = a -- (snd)

leftUnitaryMulR :: a -> (a, ())
leftUnitaryMulR a = (a,())


-----------------------------------------------------------------------------------
--13) one is unitary with respect to multiplication on the right,
-- there is no proof here. but I guess the answers should look more like this.

-- -- r(∼),(×),1 :
-- ∀x:A x×1≅x,
rightUnitaryMulR :: a -> (a, ())
rightUnitaryMulR a = (a,())

rightUnitaryMulL :: (a, ()) -> a
rightUnitaryMulL (a,()) = a


-----------------------------------------------------------------------------------
-- 14) a proof that multiplication is distributive over addition on the left,
-- l(∼),(+),(×):∀
-- x,y,z:A x×(y+z)∼x×y+x×z,

leftDistribMulR :: (x, Either y z) -> Either (x,y) (x,z)
leftDistribMulR (x,Left y)= Left (x,y)
leftDistribMulR (x, Right z) = Right (x,z)

leftDistribMulL :: Either (x,y) (x,z) -> (x, Either y z)
leftDistribMulL (Left (x,y)) = (x, Left y) 
leftDistribMulL (Right (x,z)) = (x, Right z)


-----------------------------------------------------------------------------------
--15) a proof that multiplication is distributive over addition on the right,
-- r(∼),(+),(×):∀x,y,z:A
-- (x+y)×z∼x×z+y×z

rigthDistribMulR :: ((Either x y), z) -> Either (x,z) (y,z)
rigthDistribMulR ((Left x), z)= Left (x,z)
rigthDistribMulR ((Right y), z) = Right (y,z)

rigthDistribMulL :: Either (x,z) (y,z) -> ((Either x y), z)
rigthDistribMulL (Left (x,z)) = ((Left x), z)
rigthDistribMulL (Right (y,z)) = ((Right y), z)


-----------------------------------------------------------------------------------
--16) zero is absorbing with respect to multiplication on the left,
-- l(∼),0,(×): 
-- ∀x:A 0×x∼0,

leftAbsorbMulL :: (Void, x) -> Void
leftAbsorbMulL x = case x of {}

leftAbsorbMulR ::Void -> (Void, x)
leftAbsorbMulR x = case x of {}


-----------------------------------------------------------------------------------
-- 17) zero is absorbing with respect to multiplication on the right,
-- r(∼),0,(×)
-- :∀x:A x×0∼0

rightAbsorbMulL :: (x, Void) -> Void
rightAbsorbMulL x = case x of {}

rightAbsorbMulR ::Void -> (x, Void)
rightAbsorbMulR x = case x of {}


-----------------------------------------------------------------------------------
--18) exponentiation is proper,
-- p(∼),(↑):∀x,y,z,w:A
-- x∼y → z∼w → z^x∼w^y

properExprL :: (z -> x) -> z -> x
properExprL = id

properExprR :: (z -> x) -> z -> x
properExprR = id


-----------------------------------------------------------------------------------
-- 19) exponentiation is distributive over addition and multiplication on the right,
-- r(∼),(+),(×),(↑):∀x,y,z:A
-- x^(y+z) ∼ x^y×x^z

exp :: (->) a b -> a -> b
exp f a =  f a

--Mitätapahtuu?
distExpL :: (->) (Either y z) x -> (((->) y x), ((->) z x)) -- Tämä siitä tuli kun katsoin esimerkkiä, mutta täh?!
distExpL f = ((\y -> f (Left y)),(\z -> f (Right z)))
--huui

distExpR :: (((->) y x), ((->) z x)) -> (->) (Either y z) x
distExpR (yx, zx) = \eit -> case eit of
  Left y -> yx y
  Right z -> zx z


-----------------------------------------------------------------------------------
-- 20) exponentiation is associative over multiplication on the right,
--x^(y×z)≅(x^y)^z

arrow_right_associator :: (->) (y, z) x -> (->) z ((->) y x)
arrow_right_associator = flip . curry

arrow_right_coassociator :: (->) z ((->) y x) -> (->) (y, z) x
arrow_right_coassociator = uncurry . flip


-----------------------------------------------------------------------------------
-- 21) exponentiation is distributive over multiplication on the left,
-- l(∼),(×),(↑):∀x,y,z:A
-- (x×y)^z ∼ x^z×y^z

distExpMulL :: (->) z (x,y) -> (((->) z x), ((->) z y))
distExpMulL f = (fst . f, snd . f)

distExpMulR :: (((->) z x), ((->) z y)) -> (->) z (x,y)
distExpMulR (zx, zy) = \z -> (zx z, zy z)


-----------------------------------------------------------------------------------
-- 22) one is a unit of exponentiation on the left,
-- l(∼),1,(↑):
-- ∀x:A x^1∼x

unitExpL :: (->) () x -> x
unitExpL f = f ()

unitExpR :: x -> (->) () x
unitExpR x = const x


-----------------------------------------------------------------------------------
-- 23) one is absorbing with respect to exponentiation on the right,
-- r(∼),1,(↑):∀x:A
-- 1x∼1

oneAbsorbExpL :: (->) x () -> ()
oneAbsorbExpL f = ()

oneAbsorbExpR :: () -> (->) x ()
oneAbsorbExpR () = const ()


-----------------------------------------------------------------------------------
-- 24) zero is absorbing with respect to exponentiation on the left,
-- l(∼),0,1,(↑):∀x:A
-- x^0∼1

zeroAbsorbExpL :: (->) Void x -> ()
zeroAbsorbExpL f = ()

zeroAbsorbExpR :: () -> (->) Void x
zeroAbsorbExpR () = \e -> case e of {}




