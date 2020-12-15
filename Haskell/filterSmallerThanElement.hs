main :: IO ()
main = do
  print (filterSmallerThan [1..50]19)
  print (sum (filterSmallerThan [1..50]19))


filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan [] _ = []
filterSmallerThan xs a = [x| x <- xs ,x>=a] 