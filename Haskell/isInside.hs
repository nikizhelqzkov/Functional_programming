main::IO()
main = do
    print (isInside 16 1 10)



isInside::Float -> Float -> Float -> Bool
isInside x a b = a <= x && x <= b