main :: IO ()
main = do
  print (merge [1, 2, 7, 8] [2, 3, 10])

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge xs@(a : as) ys@(b : bs)
  | a <= b = a : merge as ys
  | otherwise = b : merge xs bs