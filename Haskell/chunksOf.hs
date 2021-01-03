main :: IO ()
main = do
  print (chuncksOf 4 [1, 2, 3, 4, 5, 6])

chuncksOf :: Int -> [t] -> [[t]]
chuncksOf _ [] = []
chuncksOf k xs = take k xs : chuncksOf k (drop k xs)
