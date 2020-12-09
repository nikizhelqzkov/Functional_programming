main :: IO ()
main = do
  print (sumDel 64)
  print (sumDel 645)

sumDel :: Int -> Int
sumDel n = helper 0 (n `div` 2) + 1
  where
    helper count del
      | del < 1 = count
      | n `mod` del == 0 = helper (count + 1) (del - 1)
      | otherwise = helper count (del -1)


-- 64
--63 62 61 ..32 16 8 4 2 1 
