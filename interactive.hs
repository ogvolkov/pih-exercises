{-
putStr :: String -> IO ()
putStr s = sequence_ [putChar ch | ch <- s]
-}

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))
                    
                    
putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow n row | (n, row) <- zip [1..] board ]

adder :: IO ()
adder = do
  putStr "How many numbers? "
  countStr <- getLine
  let count = read countStr :: Int
  adderHelper 0 count

adderHelper :: Int -> Int -> IO ()

adderHelper total 0 = do
  putStr "The total is "
  putStrLn $ show total

adderHelper total count = do
  numStr <- getLine
  let num = read numStr :: Int
  adderHelper (total+num) (count-1)
  
adder' :: IO ()
adder' = do
  putStr "How man numbers? "
  countStr <- getLine
  let count = read countStr :: Int
  nums <- sequence $ replicate count readInt
  putStr "The total is "
  putStrLn (show $ sum nums)
 
readInt :: IO Int
readInt = do
  numStr <- getLine
  return (read numStr :: Int)
  
readLine :: IO String
readLine = do
  ch <- getChar
  if ch == '\n' then
    return []
  else
    do
      if ch == '\DEL' then
        do putStr "\b"
           readLine
      else
        do rest <- readLine
           return $ ch:rest
           
readLine' :: IO String
readLine' = do
  ch <- getChar
  case ch of
    '\n' ->   return []
    '\DEL' -> do putChar '\b'
                 readLine'
    _ ->      do rest <- readLine'
                 return $ ch:rest