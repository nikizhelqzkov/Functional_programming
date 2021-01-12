main :: IO ()
main = do
  print ()

type Point = (Double, Double, Double)

first :: Point -> Double
first (x, _, _) = x

second :: Point -> Double
second (_, y, _) = y

third :: Point -> Double
third (_, _, z) = z

minDistance :: [Point] -> Double
minDistance p = helper p 0
  where
    distance p1 p2 = (first p1 - first p2) * (first p1 - first p2) + (second p1 - second p2) * (second p1 - second p2) + (third p1 - third p2) * (third p1 - third p2)
    helper [] minD = minD
    helper (p : p2 : ps) minD
      | distance p p2 < minD = helper (p2 : ps) (distance p p2)
      | otherwise = helper (p2 : ps) minD