main :: IO ()
main = do
  print (inverseFun (\x -> x + 1) (\x -> x -1) 5 10)
  print (inverseFun (\x -> x * x) (\x -> x ^ 3) 0 1)
  print (inverseFun (\x -> x + 1) (\x -> x + 2) 0 1)
  print (inverseFun (\x -> x * 3) (\x -> x + 2) 2 4)

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b = and [f (g x) == x && g (f x) == x | x <- [a .. b]]
