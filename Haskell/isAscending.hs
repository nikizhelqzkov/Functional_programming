main :: IO ()
main = do
  print (isAscendingList [1, 2, 3, 4, 5, 6])
  print (isAscending 123456)

isAscendingList :: [Int] -> Bool
isAscendingList [] = True
isAscendingList [_] = True
isAscendingList (x1 : x2 : xs) = x1 < x2 && isAscendingList (x2 : xs)

isAscending :: Int -> Bool
isAscending n = isAscendingList (reverse (addToList n))
  where
    addToList n
      | n < 10 = [n]
      | otherwise = (n `mod` 10) : addToList (n `div` 10)