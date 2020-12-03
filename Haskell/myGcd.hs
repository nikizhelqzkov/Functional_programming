main :: IO ()
main = do
  print (myGcd 66 16)

myGcd :: Int -> Int -> Int
myGcd a b
  | a == b = a
  | a < b = gcd a (b - a)
  | otherwise = gcd (a - b) b