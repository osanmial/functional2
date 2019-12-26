module Week8.Exercise1 where

----    Derivative of Maybe a.
{-

Maybe a = Just a | Nothing
(a + 1)
derivative:
1

-}
type MaybeContext = ()
data MaybeZipper a = MZ (a, ())

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
data TupleContext a = TC (Bool, a) 
data TupleZipper a = TZ (a,(Bool,a))

----    Derivative of [] a.
{-
[a] 
≅ μy(1 + x × y)
D >> D_x (1 + x × y)

helper definition for list:
  [a]
≅ (μy(1 + x × y))
≅ (1 + x × (μy(1 + x × y)))
≅ (1 + x × (1 + x × (μy(1 + x × y))))
≅ (1 + x × (1 + x × (1 + x × (μy(1 + x × y)))))
≅ 1 + x × (1 + x × (1 + x × ...))
≅ 1 + x + x^2 + x^3 × ...

F := (1 + y × x)
  D_x (μy(1 + y × x))
≅ μz( (λy(D_x(1 + y × x))(μy.F)  +  (λy(D_y(1 + y × x))(μy.F)  × z)
≅ μz( (λy(y)(μy.F)  +  (λy(x)(μy.F)  × z)
≅ μz( (μy (1 + y × x))  +  x × z)
≅ μz( (μy (1 + y × x))  +  x × z)
≅ (μy (1 + y × x))  +  x × μz((μy (1 + y × x))  +  x × z)
≅ (μy (1 + y × x))  +  x × ((μy (1 + y × x))  +  x × μz((μy (1 + y × x))  +  x × z))
≅ [x] + (x×[x] + (x×[x] + (x×[x] + ... ))
≅ [x] + [x] × (x + x + x + ... ))
≅ [x] + [x] × x × (1 + 1 + 1 + ... ))
≅ (1 + x + x^2 + x^3 × ...) + (1 + x + x^2 × x^3 + ...) × x × (1+1+1...)
≅ (1 + x + x^2 + x^3 × ...) + (1 + x + x^2 × x^3 + ...) × (x+x+x...)
≅ (1 + x + x^2 + x^3 × ...) + (1 + x + x^2 × x^3 + ...) × ( x + x^2 + x^3 ...)
≅ (1 + x + x^2 × x^3 + ...) × (1 + x + x^2 + x^3 + ...)
≅ [x] × [x]

Omitted writing descriptions for operations.

-}

data ListContext a = LC ([a],[a])
data ListZipper a = LZ (a, ([a],[a]))

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
