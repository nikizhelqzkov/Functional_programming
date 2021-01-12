import Data.Char

main :: IO ()
main = do
  print (init "kozaaad" ++ "cdfs")
  print (abs (ord 'c' - ord 'C') == 32)
  print (reduceStr "dabAcCaCBAcCcaDDd")

reduceStr :: String -> String
reduceStr (s : str) = reducer str [s] s
  where
    isLowerBigger p q = abs (ord p - ord q) == 32

    reducer [] res prev = res
    reducer (s : str) res prev
      | isLowerBigger prev s = reducer (tail (init res ++ str)) [head (init res ++ str)] (head (init res ++ str))
      | otherwise = reducer str (res ++ [s]) s