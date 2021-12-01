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

        -- x = f 0 i' [] (0, 0)
        -- x = nextJmpNop 5 i'
        x = getList 0 i'
    
    print x

getList :: Int -> [String] -> Int
getList x ys = let
    x' = nextJmpNop x ys
    a = take (x') ys
    z = drop (x' + 1) ys
    i = if (words (ys!!x'))!!0 == "jmp" then "nop" else "jmp"
    ys' = a ++ [i ++ " " ++ ((words (ys!!x'))!!1)] ++ z
    n = f 0 ys' [] (0, 0)
    in if (fst n) >= length ys then snd n else getList (x + 1) ys
    -- in ys'

nextJmpNop :: Int -> [String] -> Int
nextJmpNop x ys
    | x >= length ys = -1
    | otherwise = let
        i = (words (ys!!x))!!0
        in if (i == "jmp" || i == "nop") then x else nextJmpNop (x + 1) ys

f :: Int -> [String] -> [Int] -> (Int, Int) -> (Int, Int)
f i xs ys a
    | i >= length xs = (i, (snd a))
    -- | i < 0 = (i, (snd a))
    | i `elem` ys = (i, (snd a))
    | otherwise = let
        b = words (xs!!i)
        n = getNum (b!!1)
        a' = if (b!!0) == "acc" then (snd a) + n else snd a
        i' = if (b!!0) == "jmp" then i + n else i + 1
        in f i' xs (i:ys) (i, a')

getNum :: String -> Int
getNum x = (if x!!0 == '-' then -1 else 1) * (read (tail x) :: Int)