main :: IO ()
main = do
  print (isPrime 1)
  print (isPrime 13)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
  where
    helper d
      | d == n = True
      | n `mod` d == 0 = False
      | otherwise = helper (d + 1)