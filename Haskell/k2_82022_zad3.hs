main :: IO ()
main = do
  print (splitPoints (1, 1) 5 [(1, 2), (2, 3), (10, 15), (-1, 1), (12, 14)])

type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p r [] = ([], [])
splitPoints p r xp = helper p r xp [] []
  where
    helper p r [] lr rr = (lr, rr)
    helper p r (x : xp) lr rr
      | (fst x - fst p) ^ 2 + (snd x - snd p) ^ 2 <= r ^ 2 = helper p r xp (lr ++ [x]) rr
      | otherwise = helper p r xp lr (rr ++ [x])