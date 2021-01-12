main :: IO ()
main = do
  print (reverseOrdSuff 37563)
  print (reverseOrdSuff 32763)
  print (reverseOrdSuff 32567)
  print (reverseOrdSuff 32666)

reverseOrdSuff :: Int -> Int
reverseOrdSuff el = helper (el`div`10) (el`mod`10) 0 (el`mod`10)
  where
    helper el res max prev
      | el < 10 && prev < el && (res * 10 + el) > max = res * 10 + el
      | el < 10 && prev < el && (res * 10 + el) < max = max
      | el < 10 && prev >= el && res<=max = max
      | el < 10 && prev >= el && res>max = res
      | prev >= (el `mod` 10) && res > max = helper (el `div` 10) 0 res (el `mod` 10)
      | prev >= (el `mod` 10) && res <= max = helper (el `div` 10) 0 max (el `mod` 10)
      | prev < (el `mod` 10) = helper (el `div` 10) (res * 10 + (el `mod` 10)) max (el `mod` 10)
