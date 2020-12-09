main :: IO ()
main = do
  print (sumAB 10 30 0)

sumAB :: Int -> Int -> Int -> Int
sumAB a b sum
  | a > b = sum
  | isPalindrom a = sumAB (a + 1) b (sum + a)
  | otherwise = sumAB (a + 1) b sum
  where
    isPalindrom elem = makePalindrom elem 0 == a
      where
        makePalindrom e res = if e < 10 then res * 10 + e else makePalindrom (e `div` 10) (res * 10 + e `mod` 10)


