module Week4.Exercise3 where

data Expr = Add Expr Expr | Zero | Mul Expr Expr | One
          deriving Show

(#+) = add
(#*) = mul

add :: Expr -> Expr -> Expr
add x    Zero = x
add Zero y    = y
add x    y    = Add x y

mul :: Expr -> Expr -> Expr
mul x   One = x
mul One y   = y
mul x (Add a b) = Add (Mul x a) (Mul x b)
mul (Add a b) x = Add (Mul x a) (Mul x b)
mul x   y   = Mul x y
