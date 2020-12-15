main :: IO ()
main = do
  print (incrementAllBy [1 .. 10] 2)

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy [] _ = []
incrementAllBy xs a = [x + a | x <- xs]
