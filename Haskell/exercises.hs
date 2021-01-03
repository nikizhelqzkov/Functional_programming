main :: IO ()
main = do
  print (chunksBy 3 [2, 3, 5, 8, 4, 7, 5, 5, 5, 8, 4])
  print (isTriangular [[1, 2, 3], [0, 5, 6], [0, 0, 9]])
--[1,2,3]
--[0,5,6] [1 0 0]      [0 0]        [ 5 6]    [5 0] -> [0]      [0 9]  [9]
--[0,0,9]                           [ 0 9]
--          


chunksBy :: Int -> [Int] -> [[Int]]
chunksBy _ [] = []
chunksBy n xs = take n xs : chunksBy n (drop n xs)

isTriangular :: [[Int]] -> Bool
isTriangular [] = False
isTriangular [[_]] = True
isTriangular xs = all (== 0) (tail (map head xs)) && isTriangular (map tail (tail xs))