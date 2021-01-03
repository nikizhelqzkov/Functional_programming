main :: IO ()
main = do
  print (isImage [1, 2, 3] [3, 5, 8])

isImage :: [Int] -> [Int] -> Bool
isImage [_] [_] = True
isImage (x1 : x2 : xs) (y1 : y2 : ys) = x1 - y1 == x2 - y2 && isImage (x2 : xs) (y2 : ys)