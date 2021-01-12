import Data.Char

main :: IO ()
main = do
  print (closestToAverage store1)
  print (cheaperAlternative store2)

type Product = (String, Double)

type StoreAvailability = [Product]

closestToAverage :: StoreAvailability -> String
closestToAverage p = helper p (aver p) (head p)
  where
    helper [] _ p = fst p
    helper (x : xs) av p
      | abs (av - snd x) < abs (av - snd p) = helper xs av x
      | abs (av - snd x) >= abs (av - snd p) = helper xs av p
    aver xs = sum (map snd xs) / fromIntegral (length xs)

store1 = [("bread", 1), ("milk", 2.5), ("lamb", 10), ("cheese", 5), ("butter", 2.3)]

store2 = [("bread", 1), ("cheese", 2.5), ("bread", 1), ("cheese", 5), ("butter", 2.3)]

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative (x : xs)
  | isCount x xs = 1 + cheaperAlternative xs
  | otherwise = cheaperAlternative xs
  where
    isCount el [] = False
    isCount el (x : xs)
      | (fst el == fst x) && (snd el /= snd x) = True
      | otherwise = isCount el xs