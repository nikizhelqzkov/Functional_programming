import Data.Tuple

main :: IO ()
main = do
 print (getAverageBalance (accounts1,people1) (\(_,_,city) -> city == "Burgas"))
 print (getAverageBalance (accounts1,people1) (\(_,(n:_),_) -> n == 'P'))
 print (averageBalanceOfCities (accounts1,people1) ["Sofia","Burgas","Stara Zagora"] )
 print (countInteresting t1)
 print (countInteresting t2)
 

type Account = (Int, Int, Double)

fst3 :: Account -> Int
fst3 (x, _, _) = x

snd3 :: Account -> Int
snd3 (_, y, _) = y

thrd3 :: Account -> Double
thrd3 (_, _, z) = z

type Person = (Int, String, String)

fst3P :: Person -> Int
fst3P (x, _, _) = x

snd3P :: Person -> String
snd3P (_, y, _) = y

thrd3P :: Person -> String
thrd3P (_, _, z) = z

-------------1a--------------------------------------------------------------
getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (acc, p) f = helper3 (helper2 (helper p f) acc acc [])
    where 
        helper people f = [d|d<-people,f d]
        helper2 [] acc save res = res
        helper2 people [] save res = helper2 (tail people) save save res
        helper2 people acc save res
            |fst3P(head people)==snd3 (head acc) = helper2 people (tail acc) save (head acc:res)
            |otherwise  = helper2 people (tail acc) save res
        helper3 acc = foldr ((+) . thrd3) 0.0 acc/fromIntegral (length acc) 
people1 = [(1,"Ivan","Sofia"),(2,"Georgi","Burgas"), (3,"Petar","Plovdiv"),(4,"Petya","Burgas")]
accounts1 = [(1,1,12.5),(2,1,123.2),(3,2,13.0),(4,2,50.2),(5,2,17.2),(6,3,18.3),(7,4,19.4)]        

------------------------1b--------------------------------------------------

averageBalanceOfCities  ::  ([Account],[Person]) ->  [String] -> Double
averageBalanceOfCities (acc,p) str = getAverageBalance (acc,p)(\x -> isCorrect str x)  
    where 
        isCorrect [] p  = False 
        isCorrect str p
            |thrd3P p == head str = True 
            |otherwise = isCorrect (tail str) p


----------------------------2---------------------

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