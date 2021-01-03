import Data.Char

main :: IO ()
main = do
  print ("BOOL" ++ charToString (chr (48 + 9)) ++ "koza")
  print (drop 1 "b" ++ "a")
  print (change "H" "B")
  print ("2" ++ "1")
  print (ord '9' - ord '0')
  print (show 298)
  print (zipWith (/) [1,2,3][4,5,6])
  print (tail (map head [[1,2,3],[4,5,6]]))

multiply::[Integer]->Integer->Integer 
multiply [x] = error ""
-- print ()

charToString :: Char -> String
charToString c = [c]

change :: String -> String -> String
change str strTwo = drop 1 str ++ strTwo