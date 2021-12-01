main = do
--     let i = "nop +0\n\
-- \acc +1\n\
-- \jmp +4\n\
-- \acc +3\n\
-- \jmp -3\n\
-- \acc -99\n\
-- \acc +1\n\
-- \jmp -4\n\
-- \acc +6"
    i <- readFile "day08.in"

    let i' = lines i

        x = f 0 i' [] 0
    
    print x

f :: Int -> [String] -> [Int] -> Int -> Int
f i xs ys a
    | i `elem` ys = a
    | otherwise = let
        b = words (xs!!i)
        n = getNum (b!!1)
        a' = if (b!!0) == "acc" then a + n else a
        i' = if (b!!0) == "jmp" then i + n else i + 1
        in f i' xs (i:ys) a'

getNum :: String -> Int
getNum x = (if x!!0 == '-' then -1 else 1) * (read (tail x) :: Int)