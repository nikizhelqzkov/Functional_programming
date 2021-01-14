main::IO()
main = do
    print(fn 0.5)
    print(fn (-2.0))
    

maximize::(Ord a, Num a) => [a -> a] -> (a -> a)
maximize fs x = snd (maximum [(abs(f x), f x )| f <- fs ])


fn = maximize [(\x -> x*x*x),(\x -> x+1)]