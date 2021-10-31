import Data.List(sortOn)

main :: IO ()
main = do
  print (approxSolutions [1,2,3,4,5] 184)
  -- print $ length es
  -- print $ length ves
  where
    es = [e | s <- choices [1,3,7,10,25,50], e <- exprs s]
    ves = [e | e <- es, eval e /= []]
  
data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"
  
valid'' :: Op -> Int -> Int -> Bool
valid'' Add _ _ = True
valid'' Sub x y = x > y
valid'' Mul _ _ = True
valid'' Div x y = x `mod` y == 0

valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub x y = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && (x `mod` y) == 0

valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x >= y && y /= 1 && x /= 1
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
        brak (Val n) = show n
        brak e = "(" ++ show e ++ ")"
        
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
                    
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y: ys) | x == y     = ys
                 | otherwise  = y:(remove x ys)

-- isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) ys = if ys == ys' then False else isChoice xs ys'
                     where ys' = remove x ys
                     
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n | isChoice (values e) ns = eval e == [n]
                | otherwise = False
  
split :: [a] -> [([a], [a])]
split []    = []
split [_]   = []
split (x:xs) = ([x], xs): [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
  l <- exprs ls,
  r <- exprs rs,
  e <- combine l r]
  
combine :: Expr -> Expr -> [Expr]
combine l  r = [App o l r | o <- ops]

ops:: [Op]
ops = [Add, Sub, Mul, Div, Exp]

solutions :: [Int] -> Int -> [Expr]
solutions ns n =
  [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
  
type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
    lx <- results ls,
    ry <- results rs,
    res <- combine' lx ry]
    
combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]
  
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n =
  [e | ns' <- choices ns, (e,m) <- results ns', m == n]
  
simplicity :: Expr -> Int
simplicity = length . values

sortedSolutions :: [Int] -> Int -> [Expr]
sortedSolutions ns n = sortOn simplicity (solutions' ns n)

approxSolutions :: [Int] -> Int -> [Expr]
approxSolutions ns n =
    sortOn simplicity $ map fst pick
  where
    rs = [r | ns' <- choices ns, r <- results ns']
    rsd = sortOn snd $ map (\(e,v) -> (e, abs (v-n))) rs
    dist = snd $ head rsd
    pick = takeWhile (\(e,d) -> d == dist) rsd