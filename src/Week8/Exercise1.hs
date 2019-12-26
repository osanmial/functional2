module Week8.Exercise1 where

----    Derivative of Maybe a.
{-

Maybe a = Just a | Nothing
(a + 1)
derivative:
1

-}

----    Derivative of Join (,) a.
{-

Join a = {runJoin :: p a a }
Join (,) a -> (a,a) 
-> (a×a)
-> a^2
derivative:
2a
==> (Bool, a)
-}

----    Derivative of [] a.
{-
(D_a(a (μx(a x +1)) + 1)
-> (μz2 (D_a(a (μx(a x +1)) + 1) + (D_a(a (μx(a x +1)) + 1)) × z2))

List a = a List a | []
[a] -> μx (a x + 1)
derivative
D_x(μy (x y + 1))
-> μz (λy(D_x(x y) + 1)(μy(x y + 1)) + λy(D_y(x y + 1))(μy(x y + 1)) × z)
-> μz (λy(y)(μy(x y + 1)) + λy(x(μy(x y + 1))) × z)
-> μz (μy(x y + 1) + x × z)
-> μz (μy(x y + 1) + x × z)
-> μy(x y + 1) + x × μz(μy(x y + 1) + x × z)
-> (x μy(x y + 1) + 1) + x × μz(μy(x y + 1) + x × z)
-> (x μy(x y + 1) + 1) + x × (μy(x y + 1) + x × μz(μy(x y + 1) + x × z))
-> 

-> (x μy(x y + 1) + 1) + x × (x × μy(x y + 1) + 1 + x × μz(μy(x y + 1) + x × z))

-> (x μy(x y + 1) + 1) + x + x^2 × (μy(x y + 1) + μz(μy(x y + 1) + x × z)))
-> x ×   (μy(x y + 1) + 1 + 1 + x × (μy(x y + 1) + μz(μy(x y + 1) + x × z))))
-> x ×   (2 + μy(x y + 1) + x × (μy(x y + 1) + μz(μy(x y + 1) + x × z))))


-> μy(x y + 1) × μy(x y + 1)
-> 

-> μz (D_x(x (μy(x y +1)) + 1) + (0 + 0 + D_y(μy(x y + 1) + 0) + 0) × z)
-> μz (D_x(x (μy(x y +1)) + 1) + (0 + 0 + 1 + 0) + 0) × z)
-> μz (D_x(x (μy(x y +1)) + 1) + z)

-> 1 + D_x(μy(x y +1)) + μz (1 + D_x(μy(x y +1)) + z)


-> μz (D_x(μy(x y +1)) + (D_x(μy(x y +1))) × z)
-> (D_x(μy(x y +1)) + (D_x(μy(x y +1))) × (μz (D_x(μy(x y +1)) + (D_x(μy(x y +1))) × z)))

...
-> 
-> μx (a x + 1) × μx (a x + 1)
-> μx (a x + 1) × μx (a x + 1)
-> [a]×[a]


μy(x y + 1) sarja: 1+x+x^2+x^3+... vs 1+x(1 + x (1 + x (1 + x (...))) 


Based on the ability of μy(x y + 1) being represented as a geometric series (1/(1-x))
we can see that its derivative should be (1/(1-x))^2. But we don't even have a division operation defined for μy(x y + 1) so how does this come? beyond that there are far too many symbols in the paper I don't have the slightest clue about.

the answer is apparently said to be :
[a]×[a]
-}

----    Derivative of Stream a from the streams package.
{-
data Stream a = a :> Stream a
μx (a,x)
a×(a×(a×...))
infinity

the answer is probably something like :
[a]×Stream a
-}

----    Derivative of Tree a from the binary-tree package.
{-

btree = 1 + btree^2
btr
D btree ->  2 × btree

answer: (Bool, btree)
-}


----    Derivative of Tree a from the containers package.
{-

btree = 1 + ttree^3
D btree ->  3 × ttree^2
answer: ((Bool|()), (ttree,ttree))

-}
