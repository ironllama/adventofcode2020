-- import Data.List (intercalate)

main = do
    let i = "Player 1:\n\
\9\n\
\2\n\
\6\n\
\3\n\
\1\n\
\\n\
\Player 2:\n\
\5\n\
\8\n\
\4\n\
\7\n\
\10"
    -- i <- readFile "day22.in"

    let i' = foldr f [[]] $ lines i
                where f v a@(x:xs)
                        | v == "" = []:a
                        | head v == 'P' = a
                        | otherwise = ((read v::Int):x):xs
        cs = sum $ map (length) i'
        -- z = go i' [[],[]]
        z = go i' []
        z' = concat z

    print i'
    -- print cs
    -- print $ turn i'
    -- print $ go i'
    print z
    -- mapM_ print $ go i' [[],[]]
    -- mapM_ print z
    print $ sum $ map (\v -> (z'!!v) * (cs - v)) [0..cs - 1]

go :: [[Int]] -> [[[Int]]] -> [[Int]]
go c s
    | length (c!!0) == 0 || length (c!!1) == 0 = c  -- Someone ran out of cards. Game over!
    -- | c!!0 `elem` s!!0 || c!!1 `elem` s!!1 = c  -- Someone has a repeated card state, stop recursion!
    -- | [[6,3,1,9,5],[4,7,10,8,2]] `elem` s = c  -- Someone has a repeated card state, stop recursion!
    | c `elem` s = c  -- Someone has a repeated card state, stop recursion!
    -- | ts == 100 = c
    | otherwise = go (turn c) (s ++ [c])
        -- else go (c') s where c' = turn c

turn :: [[Int]] -> [[Int]]
turn c = let
    p1 = head (c!!0)
    p2 = head (c!!1)
    p1w = [(tail (c!!0) ++ [p1, p2]), tail (c!!1)]
    p2w = [tail (c!!0), (tail (c!!1) ++ [p2, p1])]
    in if p1 <= (length $ tail (c!!0)) && p2 <= (length $ tail (c!!1))
        then let
            p1' = take p1 $ tail (c!!0)
            p2' = take p2 $ tail (c!!1)
            sub = go [p1', p2'] [[], []] -- Sub-game!
            in if length (sub!!0) /= 0 && length (sub!!1) /= 0 then p1w
                else if length (sub!!1) == 0 then p1w else p2w
        else if p1 > p2 then p1w else p2w
    -- in if p1 > p2 then p1w else p2w

-- nltos :: String -> [Int] -> String
-- nltos sep = intercalate sep $ map (show)