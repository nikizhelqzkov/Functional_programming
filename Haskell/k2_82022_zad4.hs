main::IO()
main = do
    print(isBinarySearchTree t1)

data BTree = Empty | Node Int BTree BTree


isBinarySearchTree :: BTree -> Bool
isBinarySearchTree Empty = True
isBinarySearchTree (Node v Empty Empty) = True 
isBinarySearchTree (Node v (Node vl Empty Empty) (Node vr Empty Empty)) = vl<v && vr>=v  
isBinarySearchTree (Node v (Node vl lll rl) (Node vr rlr rrr))
    |vl<v && vr>=v = isBinarySearchTree (Node vl lll rl) && isBinarySearchTree (Node vr rlr rrr)
    |otherwise  = False 


t1::BTree
t1 = Node 8 (Node 3 (Node 1 Empty Empty)
                (Node 4 Empty Empty))
            (Node 10 (Node 9 Empty Empty)
                (Node 14 Empty Empty))    
              