main::IO()
main = do
    print $ tPrintLeftRootRight t1
    print $ leaf tLeaf
    print $ leaf' tLeaf
    print $ twig (T 1 [T 2 [], T 3 []])
    print $ stick (T 1 [T 2 [T 3 [T 4 []]]])
    print $ trim tree
    print $ prune tree

data Tree = T { root :: Int, subtrees :: [Tree] } deriving Show

t1 :: Tree
t1 = T 1 [T 2 [T 5 [], T 6 [T 7 []]] , T 3 [] , T 4 []]

tLeaf::Tree
tLeaf = T 1 []

tree::Tree
tree = T 1 [T 2 [T 3 []], T 4 [T 5 [T 6 []]], T 7 [T 8 [], T 9 [T 10 [T 11 []]]]]


tPrintLeftRootRight :: Tree -> [Int]
tPrintLeftRootRight (T root subtrees) = [root] ++ concat (map tPrintLeftRootRight subtrees)

leaf::Tree -> Bool
leaf (T _ []) = True
leaf _ = False

leaf' :: Tree -> Bool
leaf' (T _ subtrees) = null subtrees

twig::Tree -> Bool
-- twig (T _ subtrees) = map leaf subtrees == [True | _ <- subtrees]
twig (T _ subtrees) = foldr (&&) True (map leaf subtrees) 


stick::Tree -> Bool
stick (T _ subtrees) = length subtrees == 0 || (length subtrees == 1 && stick (head subtrees))


trim :: Tree -> Tree
-- trim (T x ts) = T x [trim t | t <- ts, not . twig $ t] -- working
-- trim (T x ts) = T x (filter (not . twig) (map trim ts))-- not working
trim (T x ts) = T x (map trim (filter (not . twig) ts))-- working


prune :: Tree -> Tree
prune t@(T x []) = t
prune t@(T x ts) = T x (if stick t then let [T y _] = ts in [T y []] else map prune ts)
-- what does let [T y _] = ts in [T y []]? 
-- it takes the first element of ts and makes a tree with it as a root and empty list of subtrees





