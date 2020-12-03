main::IO()
main = do
    print (myMin 20 30)

myMin::Int -> Int -> Int
myMin x y = if x < y then x else y