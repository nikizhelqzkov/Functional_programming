main :: IO ()
main = do
   print $ maxByLength  [[1,2,3],[1,2,3,4],[1,2,3,4,5]]
   print $ maxChain ["Boyan"] 15 "Maria" dues
   print $ maxCircle dues



dues :: [(String,Int,String)]
dues = [("Georgi",10,"Boyan"),("Boyan",15,"Sia"),("Sia",15,"Maria"),("Maria",10,"Georgi"),("Maria",10,"Petar"),("Petar",10,"Georgi"),("Boyan",10,"Maria")]

maxByLength  :: [[a]] -> [a]
maxByLength  = foldr (\x y -> if length x > length y then x else y) []


maxChain:: [String] -> Int -> String -> [(String,Int,String)] -> [String]
maxChain chain@(last:rest) amount final dues
    | last == final  = chain
    | otherwise = maxByLength [maxChain (x:chain) amount final dues | 
                                (xLast,xAmount,x)<- dues, amount == xAmount, 
                                last == xLast, not (x `elem` rest)]

maxCircle :: [(String,Int,String)] -> [String]
maxCircle dues = maxByLength [maxChain [x] xAmount final dues | (final,xAmount,x) <- dues]








