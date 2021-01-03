main :: IO ()
main = do
  print (iSort[1,5,5,2,2,212,1,45,21,1,5,2,4,4,3])

iSort :: [Int] -> [Int]
-- iSort xs = foldr ins [] xs
iSort [] = []
iSort (x:xs) = ins x (iSort xs)
ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y : ys)
  | x <= y = x : (y : ys)
  | otherwise = y : ins x ys