main :: IO ()
main = do
  print (orderedTree tree)
  print (orderedTree tree2)

data BTree = NullT | Node (Float, Float) BTree BTree

orderedTree :: BTree -> Bool
orderedTree NullT = True
orderedTree (Node (_, _) NullT NullT) = True
orderedTree (Node (a, b) NullT (Node (a1, b1) NullT NullT)) = (a <= a1) && (b > b1)
orderedTree (Node (a, b) (Node (a1, b1) NullT NullT) NullT) = (b - a) > (b1 - a1)
orderedTree (Node (x1, y1) (Node (x2, y2) ll rl) (Node (x3, y3) lr rr))
  | (y1 - x1) > (y2 - x2) && (y1 - x1) < (y3 - x3) = orderedTree (Node (x2, y2) ll rl) && orderedTree (Node (x3, y3) lr rr)
  | otherwise = False

tree = (Node (3.0, 10.0) (Node (5.0, 8.0) (Node (6.0, 7.0) NullT NullT) (Node (4.0, 9.0) NullT NullT)) (Node (2.0, 12.0) NullT (Node (1.0, 15.0) NullT NullT)))

tree2 = (Node (3.0, 10.0) (Node (5.0, 8.0) (Node (6.0, 7.0) NullT NullT) (Node (7.0, 9.0) NullT NullT)) (Node (2.0, 12.0) NullT (Node (1.0, 15.0) NullT NullT)))
