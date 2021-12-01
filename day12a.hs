main = do
--     let i = "F10\n\
-- \N3\n\
-- \F7\n\
-- \R90\n\
-- \F11"
    i <- readFile "day12.in"

    let i' = lines i

        y = move i' (0, 0) 90
        z = (abs $ fst y) + (abs $ snd y)

    -- print y
    print z

move :: [String] -> (Int, Int) -> Int -> (Int, Int)
move [] c _ = c
move (i:is) (x, y) r = let
    a = read(tail i) :: Int
    -- r' = if head i == 'L' then move is (r-a) `mod` 360
    --         else if head i == 'R' then move is (r+a) `mod` 360
    --         else r
    m = if head i == 'F'
            then case r of
                0 -> 'N'
                90 -> 'E'
                180 -> 'S'
                270 -> 'W'
            else head i
    in case m of
        'N' -> move is (x, y+a) r
        'E' -> move is (x+a, y) r
        'S' -> move is (x, y-a) r
        'W' -> move is (x-a, y) r
        'L' -> move is (x, y) ((r - a) `mod` 360)
        'R' -> move is (x, y) ((r + a) `mod` 360)