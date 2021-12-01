main = do
--     let i = "Player 1:\n\
-- \9\n\
-- \2\n\
-- \6\n\
-- \3\n\
-- \1\n\
-- \\n\
-- \Player 2:\n\
-- \5\n\
-- \8\n\
-- \4\n\
-- \7\n\
-- \10"
    i <- readFile "day22.in"

    let i' = foldr f [[]] $ lines i
                where f v a@(x:xs)
                        | v == "" = []:a
                        | head v == 'P' = a
                        | otherwise = ((read v::Int):x):xs
        cs = sum $ map (length) i'
        z = concat $ go i'

    print i'
    -- print cs
    -- print $ turn i'
    -- print $ go i'
    print z
    print $ sum $ map (\v -> (z!!v) * (cs - v)) [0..cs - 1]

go :: [[Int]] -> [[Int]]
go c = if length (c!!0) == 0 || length (c!!1) == 0 then c  -- Someone ran out of cards. Game over!
        else go $ turn c

turn :: [[Int]] -> [[Int]]
turn c = let
    p1 = head (c!!0)
    p2 = head (c!!1)
    in if p1 > p2 then [(tail (c!!0) ++ [p1, p2]), tail (c!!1)] else [tail (c!!0), (tail (c!!1) ++ [p2, p1])]