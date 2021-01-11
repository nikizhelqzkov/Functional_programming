main :: IO ()
main = do
  print (Triangal 1 2 3)

data Shape = Circle Double | Rectangle Double Double | Triangal Double Double Double
  deriving (Show)

-- instance Show Shape where
--   show (Triangal a b c) = "a = " ++ show a ++ ", b = " ++ show b ++ ", c = " ++ show c
--   show (Rectangle a b) = "a = " ++ show a ++ ", b = " ++ show b
--   show (Circle r) = "r = " ++ show r

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangal a b c) = a + b + c

area :: Shape -> Double
area (Circle r) = pi * (r ** 2)
area (Rectangle a b) = a * b
area (Triangal a b c) = sqrt (p * (p - a) * (p - b) * (p - c))
  where
    p = (a + b + c) / 2

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound _ = False

sumArea :: [Shape] -> Double
sumArea xs = sum (map area xs)

biggestArea::[Shape]->Shape 
biggestArea [] = error "Empty list"
biggestArea xs = foldl1 (\ x y -> if area x > area y then x else y) xs
          