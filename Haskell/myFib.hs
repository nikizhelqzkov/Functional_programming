main :: IO ()
main = do
  print (myFib 4)
  print (myFibIter 4)

myFib :: Int -> Int
myFib n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = myFib (n -1) + myFib (n -2)

myFibIter :: Int -> Int
myFibIter n = myf 0 0 1
  where
    myf count s current
      | count > n = s
      | otherwise = myf (count + 1) (current + s) s
