main :: IO ()
main = do
  print (findNb 1071225)
  print (findNb 40539911473216)
  print (findNb 91716553919377)

findNb :: Integer -> Integer
findNb m = helper m 1
  where
    helper m n
      | m - (n * n * n) == 0 = n
      | (m - (n * n * n)) < 0 = -1
      | otherwise  = helper (m - (n * n * n)) (n + 1)
