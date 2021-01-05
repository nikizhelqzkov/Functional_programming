import Data.Tuple

main :: IO ()
main = do
  print (chalgaZone ("hello", 26))
  print (printTriple ("hello", 25, "Maiko"))
  print (fst3' (swap3(123, 456, 789)))

type V = (String, Int, String)

type K = (Int, Int, Int)

type Music = (String, Int)

fst3 :: V -> String
fst3 (x, _, _) = x

fst3' :: K -> Int
fst3' (x, _, _) = x

snd3 :: V -> Int
snd3 (_, y, _) = y

snd3' :: K -> Int
snd3' (_, y, _) = y

thrd3 :: V -> String
thrd3 (_, _, z) = z

thrd3' :: K -> Int
thrd3' (_, _, z) = z

swap3::K->K
swap3 (x,y,z) = (z,y,x)
reverse3::V->V
reverse3 (x,y,z) = (z,y,x)
chalgaZone :: Music -> String
chalgaZone (x, y) = x ++ show y

printTriple :: V -> String
printTriple x = fst3 x ++ " " ++ show (snd3 x) ++ " " ++ thrd3 x