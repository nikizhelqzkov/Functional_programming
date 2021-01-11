main :: IO ()
main = do
  print (countInteresting t1)
  print (countInteresting t2)
  print (height t1)
  print (size t1)
  print (sumLeaves t1)
  print (inOrder t1)
  print (inOrder (mirrorTree t1))

data BTree = Empty | Node Int BTree BTree
  deriving (Show)

countInteresting :: BTree -> Int
countInteresting Empty = 0
countInteresting (Node 1 Empty Empty) = 1
countInteresting (Node 2 treeL Empty) = 1 + countInteresting treeL
countInteresting (Node 2 Empty treeR) = 1 + countInteresting treeR
countInteresting (Node 4 treeL treeR) = 1 + countInteresting treeL + countInteresting treeR
countInteresting (Node _ treeL treeR) = countInteresting treeL + countInteresting treeR

t1 :: BTree
t1 = Node 16 (Node 0 Empty Empty) (Node 4 (Node 1 Empty Empty) (Node 0 Empty Empty))

t2 :: BTree
t2 = Node 4 (Node 0 Empty Empty) (Node 2 (Node 1 Empty Empty) Empty)

height :: BTree -> Int
height Empty = 0
height (Node root l r) = 1 + max (height l) (height r)

size :: BTree -> Int
size Empty = 0
size (Node root l r) = 1 + size l + size r

sumTree :: BTree -> Int
sumTree Empty = 0
sumTree (Node root l r) = root + sumTree l + sumTree r

sumLeaves :: BTree -> Int
sumLeaves Empty = 0
sumLeaves (Node root Empty Empty) = root
sumLeaves (Node _ l r) = sumLeaves l + sumLeaves r

inOrder :: BTree -> [Int]
inOrder Empty = []
inOrder (Node root l r) = inOrder l ++ [root] ++ inOrder r

average :: BTree -> Double
average xs = fromIntegral (sumTree xs) / fromIntegral (size xs)

mirrorTree :: BTree -> BTree
mirrorTree Empty = Empty
mirrorTree (Node root l r) = Node root (mirrorTree r) (mirrorTree l)