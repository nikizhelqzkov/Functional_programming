main::IO()
main = do
    print (myMaxDivisor 14)

myMaxDivisor::Int->Int
myMaxDivisor x  = helper (x-1)
        where 
            helper d = if (x `mod` d) == 0 then d else helper (d - 1)