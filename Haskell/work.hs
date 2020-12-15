main :: IO ()
main = do
  print (incrementAllBy [1, 2, 3, 4, 5] 3)
  print (multiplyAllBy [1, 2, 3, 4, 5] 3)

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs el =
  if null xs
    then []
    else (head xs + el) : incrementAllBy (tail xs) el

multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs el =
  if null xs
    then []
    else (head xs * el) : incrementAllBy (tail xs) el