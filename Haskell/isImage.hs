main :: IO ()
main = do
  print (isImage [1,2,4,4][1,1,2,1])

isImage :: [Int] -> [Int] -> Bool
isImage [] [_] = False
isImage [_] [] = False
isImage xs ys = helper xs ys 0
  where
    helper xs ys count
      | null xs || null ys = False
      | head xs == head ys * count = True
      | otherwise = helper (tail xs) (tail ys) (count + 1)