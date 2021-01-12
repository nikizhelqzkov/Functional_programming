main :: IO ()
main = do
  print (sumUnique [[1, 2, 3, 2], [-4, -4], [5]])
  print (sumUnique [[2, 2, 2], [3, 3, 3], [4, 4, 4]])
  print (sumUnique [[1, 2, 3], [4, 5, 6], [7, 8, 9]])

sumUnique :: [[Int]] -> Int
sumUnique xs = sum (map (sum . uniquedList) xs) 
    where
    uniquedList xs = helper xs [] []
    helper [] unique res = unique
    helper (x : xs) unique res
        | isUnique x xs && isUnique x res = helper xs (x : unique) res
        | otherwise = helper xs unique (x : res)
    isUnique el [] = True
    isUnique el (x : xs)
        | el /= x = isUnique el xs
        | otherwise = False