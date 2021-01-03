import Data.Char

main :: IO ()
main = do
  print (decode "z3caa3b")

charToString :: Char -> String
charToString c = [c]

fromIntToString :: Int -> String
fromIntToString num = h num ""
  where
    h n res
      | n < 10 = res ++ charToString (chr (ord '0' + n))
      | otherwise = h (n `div` 10) res ++ charToString (chr (ord '0' + n `mod` 10))

decode :: String -> String
decode str = helper str "" 0 False
  where
    helper word res count isHasDigit
      | null word = res
      | isDigit (head word) = helper (tail word) res (count * 10 + (ord (head word) - ord '0')) True
      | isLetter (head word) && count > 0 && isHasDigit = helper word (res ++ charToString (head word)) (count - 1) True
      | isLetter (head word) && count == 0 && isHasDigit = helper (tail word) res count False
      | isLetter (head word) && count == 0 && not isHasDigit = helper (tail word) (res ++ charToString (head word)) count False