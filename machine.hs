data Expr = Val Int | Add Expr Expr | Mul Expr Expr

type Cont = [Op]

data Op = EVAL Expr | ADD Int | MEVAL Expr | MUL Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)
eval (Mul x y) c = eval x (MEVAL y: c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n+m)
exec (MEVAL y: c) n = eval y (MUL n: c)
exec (MUL n: c) m = exec c (n*m)

value :: Expr -> Int
value e = eval e []