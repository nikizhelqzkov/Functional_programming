main :: IO ()
main = do
  print (myGcd 66 16)

myGcd :: Int -> Int -> Int
myGcd a b
  | a == b = a
  | a < b = myGcd a (b - a)
  | otherwise = myGcd (a - b) b