main :: IO ()
main = do
    print (divisiors 64)


divisiors:: Integer -> [Integer]
divisiors n = [d|d <- [1..n],n`mod`d==0]     