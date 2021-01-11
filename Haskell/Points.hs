main :: IO ()
main = do
  print ()

data Point = Point2D Double Double | Point3D Double Double Double
  deriving (Eq, Show)

distance :: Point -> Point -> Double
distance Point2D {} Point3D {} = error "2d and 3d are not competible"
distance Point3D {} Point2D {} = error "2d and 3d are not competible"
distance (Point3D x1 y1 z1) (Point3D x2 y2 z2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2)
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

getClosestPoint :: [Point] -> Point -> Point
getClosestPoint [] p = error "Empty list"
getClosestPoint xs p = foldl1 (\p1 p2 -> if distance p1 p < distance p2 p then p1 else p2) xs
