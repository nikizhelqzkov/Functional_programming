main :: IO ()
main = do
  print (countInteresting t1)
  print (countInteresting t2)
  print (countInteresting t)

data BTree = Empty | Node Int BTree BTree

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
