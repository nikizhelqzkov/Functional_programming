main::IO()
main = do
    print $ treeWords treeString
    print $ quickSort [1,5,2,3,4,6,7,8,9,10]
    print $ quickSort' [1,5,2,3,4,6,7,8,9,10]
    print $ "a" : [ ['b', 'c'], "d" ]




data Tree a = EmptyTree | Node {
value :: a,
left :: Tree a,
right :: Tree a
} deriving (Show,Read)

tree::Tree Int
tree = Node 5 (Node 3 (Node 1 EmptyTree EmptyTree) (Node 4 EmptyTree EmptyTree)) (Node 7 (Node 6 EmptyTree EmptyTree) (Node 8 EmptyTree EmptyTree))

treeString::Tree Char
treeString = Node 'a' (Node 'b' (Node 'c' EmptyTree EmptyTree) (Node 'd' EmptyTree EmptyTree)) (Node 'e' (Node 'f' EmptyTree EmptyTree) (Node 'g' EmptyTree EmptyTree))


treeWords :: Tree Char -> [String]
treeWords EmptyTree = []
treeWords (Node v EmptyTree EmptyTree) = [[v]]
treeWords (Node v l r) = map (v:) (wl ++ wr)
    where
        wl = treeWords l
        wr = treeWords r


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = lesser ++ [x] ++ greater 
    where
        lesser = quickSort( filter (<x) xs)
        greater = quickSort ( filter (>=x) xs)

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = quickSort'(lesser) ++ [x] ++ quickSort'(greater) 
    where
        lesser =  filter (<x) xs
        greater = filter (>=x) xs

