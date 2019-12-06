{-# LANGUAGE EmptyCase #-}

module Week6.Exercise3 where
import Data.Void

arrow_right_associator :: ((b, c) -> a) -> c -> (b -> a)
arrow_right_associator = flip . curry

arrow_right_coassociator :: (c -> (b -> a)) -> (b, c) -> a
arrow_right_coassociator = uncurry . flip





--1) a proof that equivalence is reflexive, 
-----------------------------------------------------------------------------------
--2) a proof that equivalence is symmetric, 
-----------------------------------------------------------------------------------
-- 3)a proof that equivalence is transitive, 
-----------------------------------------------------------------------------------
--4) a proof that addition is proper, 
-----------------------------------------------------------------------------------
--5) a proof that addition is associative, 
-----------------------------------------------------------------------------------
--6) a proof that addition is commutative, 
-----------------------------------------------------------------------------------
--7) a proof that zero is unitary with respect to addition on the left, 
-----------------------------------------------------------------------------------
--8) a proof that zero is unitary with respect to addition on the right, 
-----------------------------------------------------------------------------------
--9)a proof that multiplication is proper, 
-----------------------------------------------------------------------------------
--10) a proof that multiplication is associative, 
-----------------------------------------------------------------------------------
--11) a proof that multiplication is commutative,

-----------------------------------------------------------------------------------
--12) a proof that one is unitary with respect to multiplication on the left,

-- one is unitary with respect to multiplication on the left
-- l(∼),(×),1 : ∀x:A 1×x ≅ x,
{-
funext: ∀A,B:U ∀f,g:A→B (∀x:A f(x)=g(x))→f=g

(≅):U×U→U
A≅B ≡ ∃f:A→B ∃g:B→A g∘f=id∧f∘g=id

∃f:A→B ∃g:B→A g∘f=id∧f∘g=id

∃f: ((),A)->A  ∃g: A->(A,()) g∘f=id∧f∘g=id

  f : (1, A) -> A
f(m) ≡ (1,z) ↦ m (z) 1.rule
  g : A -> (1, A)
g(m) ≡ z ↦ m (1,z) 2.rule

∀m:1×C→A ∀(z):C  (g∘f)(m)(z) = id(m)(z)

m and k are arbitrary inhabitants of ther definition

-- Tässä vissiin tarvittaisiin lambdacalculusta että laskusäännöt olisivat täysin yksiselitteiset?
(g∘f)(m)(k) = id(m)(k)
(g(f(m)))(k) = m(k) 
(z ↦ f(m) (1,z))(k) = m(k) 2.rule
f(m) (1,k) = m(k)
((1,z) ↦ m (z))(1,k) = m(k) 1.rule
m(k) = m(k)

-- toimii toisinkinpäin

-}

leftUnitaryMulL :: ((), a) -> a
leftUnitaryMulL ((),a) = a -- (snd)

leftUnitaryMulR :: a -> (a, ())
leftUnitaryMulR a = (a,())  


-----------------------------------------------------------------------------------
--13) a proof that one is unitary with respect to multiplication on the right,

-- one is unitary with respect to multiplication on the right,
-- r(∼),(×),1 : ∀x:A x×1≅x,
unitaryRightMul :: a -> (a, ())
unitaryRightMul a = (a,()) 


-----------------------------------------------------------------------------------
-- 14) a proof that multiplication is distributive over addition on the left, 
-----------------------------------------------------------------------------------
--15) a proof that multiplication is distributive over addition on the right, 
-----------------------------------------------------------------------------------
--16)a proof that zero is absorbing with respect to multiplication on the left, 
-----------------------------------------------------------------------------------
--17)a proof that zero is absorbing with respect to multiplication on the right, 
-----------------------------------------------------------------------------------
--18)a proof that exponentiation is proper, 
-----------------------------------------------------------------------------------
--19)a proof that exponentiation is distributive over addition and multiplication on the right, 
-----------------------------------------------------------------------------------
-- 20)a proof that exponentiation is associative over multiplication on the right, 
-----------------------------------------------------------------------------------
--21) a proof that exponentiation is distributive over multiplication on the left, 
-----------------------------------------------------------------------------------
--22) a proof that one is a unit of exponentiation on the left, 
-----------------------------------------------------------------------------------
-- a proof that one is absorbing with respect to exponentiation on the right, 
-----------------------------------------------------------------------------------
-- a proof that zero is absorbing with respect to exponentiation on the left, 

