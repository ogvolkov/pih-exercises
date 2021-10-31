altMap :: (a->b) -> (a->b)-> [a] -> [b]

altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble :: Int -> Int
luhnDouble x = if x*2 > 9 then x*2-9 else x*2

luhn :: [Int] -> Bool
luhn xs = (sum $ altMap id luhnDouble $ reverse xs) `mod` 10 == 0 