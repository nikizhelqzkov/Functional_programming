main :: IO ()
main = do
    print (prodSumDiv [1,2,3,4,5]2)



summing::Integer -> Integer
summing el = sum [d|d<-[1..el],el`mod`d==0]

prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv [] _ = 0
prodSumDiv [_] 0 = 0
prodSumDiv xs k = product(filter (\x -> summing x `mod`k==0) xs)
