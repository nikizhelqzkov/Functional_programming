main :: IO ()
main = do
  print (multiplyAllBy [1 .. 10] 2)

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy [] _ = []
multiplyAllBy (x : xs) a = (x * a) : multiplyAllBy xs a