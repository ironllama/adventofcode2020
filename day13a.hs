import Data.List.Split

main = do
--     let i = "939\n\
-- \7,13,x,x,59,x,31,19"
    i <- readFile "day13.in"

    let i' = lines i
        x = read (head i') :: Int
        ys = foldr (\a acc -> if a /= "x" then (read a :: Int):acc else acc ) [] $ splitOn "," $ last i'
        z = findMatch x ys
        z' = (fst z - x) * snd z

    print i'
    print ys
    print z
    print z'

findMatch :: Int -> [Int] -> (Int, Int)
findMatch x ys = let
    a = foldr (\y a -> if snd a == -1 && x `mod` y == 0 then (x, y) else a) (0, -1) ys
    in if snd a /= -1 then a
        else findMatch (x+1) ys