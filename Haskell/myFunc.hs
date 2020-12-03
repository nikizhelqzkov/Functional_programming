main::IO()
main = do
    print (myFunc 20 10)

myFunc::Double ->Double->Double
myFunc a b = (a*a + b*b)/2
