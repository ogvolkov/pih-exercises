data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n-1)

add :: Nat -> Nat -> Nat
add a Zero = a
add a (Succ b) = Succ (add a b)

mul :: Nat -> Nat -> Nat
mul a Zero = Zero
mul a (Succ b) = add a (mul a b)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving(Show)

occurs :: Ord a => a -> Tree a -> Bool

occurs a (Leaf b) = compare a b == EQ

occurs a (Node left root right) =
  case compare a root of
    EQ -> True
    LT -> occurs a left
    GT -> occurs a right
--         10
--     5        13
--  2    6    11  12
--

t =  Node (Node (Leaf 2) 5 (Leaf 6)) 10 (Node (Leaf 11) 13 (Leaf 12))

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node left _ right) = leaves left + leaves right + 1

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left _ right) = abs (leaves left - leaves right) <= 1

--         10
--     5        13
--  2    6    
--

t2 =  Node (Node (Leaf 2) 5 (Leaf 6)) 10 (Leaf 13)

split :: [a] -> ([a],[a])
split xs = (take h xs, drop h xs) where h = length xs `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (head r) (balance $ tail r) where (l,r) = split xs

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

folde f _ (Val x) = f x
folde f g (Add l r) = g (folde f g l) (folde f g r)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+) 

{-
instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Nothing == (Just x) = False
  (Just x) == Nothing = False
  (Just x) == (Just y) = x == y
-}