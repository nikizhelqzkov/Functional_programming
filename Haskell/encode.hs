import Data.Char

main :: IO ()
main = do
  print (encode "aaaaaaaaaaaaacccccccccccccccccccbbbbbbbbbbbbbbbbbddwfsa")
 -- 0 "" ""
 -- 1 a ""
 -- 2 a ""
 -- ...
 -- 13 a ""
 -- 1 c "13a"
 -- 19 c "13a"
 -- 13a19c17b
 -- 1 d "13a19c17b"
 -- 2 d "13a19c17b"
 -- 1 w "13a19c17bdd"
 -- 1 f "13a19c17bddw"
 -- 1 s "13a19c17bddws"
 -- 1 а "13a19c17bddwsа"
charToString :: Char -> String
charToString c = [c]

fromIntToString :: Int -> String
fromIntToString num = h num ""
  where
    h n res
      | n < 10 = res ++ charToString (chr (ord '0' + n))
      | otherwise = h (n `div` 10) res ++ charToString (chr (ord '0' + n `mod` 10))

encode :: String -> String
encode str = helper str 0 "" ""
  where
    helper word count cur res
      | null word && count == 1 = res ++ cur
      | null word && count == 2 = res ++ cur ++ cur
      | null word && count > 2 = res ++ fromIntToString count ++ cur
      | count == 0 = helper (tail word) (count + 1) (charToString (head word)) res
      | charToString (head word) == cur = helper (tail word) (count + 1) cur res
      | charToString (head word) /= cur && count == 1 = helper (tail word) 1 (charToString (head word)) (res ++ cur)
      | charToString (head word) /= cur && count == 2 = helper (tail word) 1 (charToString (head word)) (res ++ cur ++ cur)
      | charToString (head word) /= cur && count > 2 = helper (tail word) 1 (charToString (head word)) (res ++ fromIntToString count ++ cur)