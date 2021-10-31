import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2: int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity xs = (sum xs) `mod` 2

withParity :: [Bit] -> [Bit]
withParity xs = parity xs : xs

checkParity :: [Bit] -> [Bit]
checkParity xs | head xs == parity (tail xs) = tail xs
               | otherwise = error "Parity fail"

encode :: String -> [Bit]
encode = concat . map (withParity . make8 . int2bin . ord)

chop :: Int -> [Bit] -> [[Bit]]
chop n [] = []
chop n bits = take n bits : chop n (drop n bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . (map checkParity) . (chop 9)

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail