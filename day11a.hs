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
    i <- readFile "day11.in"

    let i' = lines i

        -- Padding top, left, right, and bottom with floor (.) so that I don't have to worry about the edges
        bi = (take (length (i'!!0)) (repeat '.'):i') ++ [take (length (i'!!0)) (repeat '.')]
        bi' = map (\a -> ('.':a) ++ ['.']) bi

        -- j = adjOccupiedRow 1 "#.#.LL###."
        -- j = adjOccupied bi'
        j = untilStable bi' 0
        z = sum $ map (length . filter (=='#')) j


    -- print i'
    -- print bi'
    -- print j
    print z

untilStable :: [String] -> Int -> [String]
untilStable is n = let
    n' = n + 1  -- This is just to count the number of times it takes for stability. Not required.
    a = adjOccupied is
    -- in if a == is then n' else untilStable a n'
    in if a == is then a else untilStable a n'  -- Just repeating until results are the same (stable)

adjOccupied :: [String] -> [String]
adjOccupied is = map (\y ->  -- Process an entire grid
                        map (\x -> let  -- Get a character, but only processes a check if it is a seat (L or #)
                            c = (is!!y)!!x
                            in if c == 'L' && adjOccupiedAround x y is == 0 then '#'
                                else if c == '#' && adjOccupiedAround x y is >= 4 then 'L'
                                else c
                        ) [0..(length (is!!y) - 1)]
                    ) [0..(length is - 1)]

adjOccupiedAround :: Int -> Int -> [String] -> Int
adjOccupiedAround x y is = let  -- Checks the 3 rows around the seat.
    t = adjOccupiedRow x (is!!(y-1))  -- Row above
    b = adjOccupiedRow x (is!!(y+1))  -- Row below
    l = if (is!!y)!!(x-1) == '#' then 1 else 0  -- Immediate left
    r = if (is!!y)!!(x+1) == '#' then 1 else 0  -- Immediate right
    in t + b + l + r

-- adjOccupiedAround x y is = let
--     t = if y > 0 then adjOccupiedRow x (y - 1) (is!!(y - 1)) else 0
--     b = if y < (length (is!!y) - 1) then adjOccupiedRow x (y + 10 (is!!(y + 1)) else 0
--     m = 

adjOccupiedRow :: Int -> String -> Int
-- adjOccupiedRow x ys = let
--     s = if x > 0 then x - 1 else 0
--     e = if x < (length ys - 1) then x + 1 else (length ys - 1)
--     in foldr (\y a -> if ys!!y == '#' then a + 1 else a) 0 [s..e]
adjOccupiedRow x ys = foldr (\x' a -> if ys!!x' == '#' then a + 1 else a) 0 [x-1..x+1]