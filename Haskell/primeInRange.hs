main :: IO ()
main = do
    print (primeInRange 12 56)


isPrime::Int->Bool
isPrime n = [1,n]== [d|d<- [1..n], n`mod`d==0]

primeInRange:: Int->Int->[Int]
primeInRange a b  = [d|d<- [a..b], isPrime d]