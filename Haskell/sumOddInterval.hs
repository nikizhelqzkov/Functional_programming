main :: IO ()
main = do
  print (sumOdd 1 10)

sumOdd :: Int -> Int -> Int
sumOdd a b
  | a > b = 0
  | a `mod` 2 == 1 = a + sumOdd (a + 1) b
  | otherwise = sumOdd (a + 1) b