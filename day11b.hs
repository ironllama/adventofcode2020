main = do
--     let i = "L.LL.LL.LL\n\
-- \LLLLLLL.LL\n\
-- \L.L.L..L..\n\
-- \LLLL.LL.LL\n\
-- \L.LL.LL.LL\n\
-- \L.LLLLL.LL\n\
-- \..L.L.....\n\
-- \LLLLLLLLLL\n\
-- \L.LLLLLL.L\n\
-- \L.LLLLL.LL"
--     let i = "#.##.##.##\n\
-- \#######.##\n\
-- \#.#.#..#..\n\
-- \####.##.##\n\
-- \#.##.##.##\n\
-- \#.#####.##\n\
-- \..#.#.....\n\
-- \##########\n\
-- \#.######.#\n\
-- \#.#####.##"
    i <- readFile "day11.in"

    let i' = lines i

        -- Padding top, left, right, and bottom with floor (.) so that I don't have to worry about the edges
        bi = (take (length (i'!!0)) (repeat '.'):i') ++ [take (length (i'!!0)) (repeat '.')]
        bi' = map (\a -> ('.':a) ++ ['.']) bi

        -- j = adjOccupied bi'
        j = untilStable bi' 0
        -- j2 = untilStable j 0
        -- j3 = untilStable j2 0
        -- o = adjOccupiedAround 1 1 bi'
        z = sum $ map (length . filter (=='#')) j


    -- print i'
    -- print bi'
    -- mapM_ putStrLn j
    -- mapM_ putStrLn j2
    -- mapM_ putStrLn j3
    -- print o
    print z

untilStable :: [String] -> Int -> [String]
untilStable is n = let
    n' = n + 1  -- This is just to count the number of times it takes for stability. Not required.
    a = adjOccupied is
    -- in if a == is then n' else untilStable a n'
    in if a == is then a else untilStable a n'  -- Just repeating until results are the same (stable)
    -- in a

adjOccupied :: [String] -> [String]
adjOccupied is = map (\y ->  -- Process an entire grid
                        map (\x -> let
                            c = (is!!y)!!x  -- Get a character, but only processes a check if it is a seat (L or #)
                            in if c == 'L' && adjOccupiedAround x y is == 0
                                then '#'
                                else if c == '#' && adjOccupiedAround x y is >= 5
                                    then 'L'
                                else c
                        ) [0..(length (is!!y) - 1)]
                    ) [0..(length is - 1)]

adjOccupiedAround :: Int -> Int -> [String] -> Int
adjOccupiedAround x y is = let  -- Checks around the seat in all directions.
    in length $ filter (==True) $ map (\d -> adjOccupiedDir x y d is) ["ul", "u", "ur", "l", "r", "dl", "d", "dr"]

adjOccupiedDir :: Int -> Int -> String -> [String] -> Bool
adjOccupiedDir x y d is = let  -- Uses the d to check that direction before figuring out whether to return or recurse
        x' = if 'r' `elem` d then x + 1 else (if 'l' `elem` d then x - 1 else x)
        y' = if 'd' `elem` d then y + 1 else (if 'u' `elem` d then y - 1 else y)
        in case ((is!!y')!!x') of
            'L' -> False
            '#' -> True
            '.' -> if x' == 0  -- Boundary checking
                || x' == (length(is!!0) - 1)
                || y' == 0
                || y' == (length is - 1)
                then False
                else adjOccupiedDir x' y' d is

-- adjOccupiedDir x y d is
--     | x == 0 || x == (length(is!!0) - 1) = False
--     | y == 0 || y == (length is - 1) = False
--     | (is!!y)!!x == 'L' = False
--     | (is!!y)!!x == '#' = True
--     | otherwise = let
--         x' = if 'r' `elem` d then x + 1 else (if 'l' `elem` d then x - 1 else x)
--         y' = if 'd' `elem` d then y + 1 else (if 'u' `elem` d then y - 1 else y)
--         in adjOccupiedDir x' y' d is