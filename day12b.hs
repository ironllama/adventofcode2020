main = do
--     let i = "F10\n\
-- \N3\n\
-- \F7\n\
-- \R90\n\
-- \F11"
    i <- readFile "day12.in"

    let i' = lines i

        y = move i' (0, 0) (10, 1)
        z = (abs $ fst y) + (abs $ snd y)

    -- print y
    print z

move :: [String] -> (Int, Int) -> (Int, Int) -> (Int, Int)
move [] c _ = c
move (i:is) (x, y) (wx, wy) = let
    a = read(tail i) :: Int
    in case head i of
        'F' -> move is ((a * wx) + x, (a * wy) + y) (wx, wy)
        'N' -> move is (x, y) (wx, wy+a)
        'E' -> move is (x, y) (wx+a, wy)
        'S' -> move is (x, y) (wx, wy-a)
        'W' -> move is (x, y) (wx-a, wy)
        'L' -> case a of
            0 -> move is (x, y) (wx, wy)
            90 -> move is (x, y) ((-wy), wx)
            180 -> move is (x, y) ((-wx), (-wy))
            270 -> move is (x, y) (wy, (-wx))
        'R' -> case a of
            0 -> move is (x, y) (wx, wy)
            90 -> move is (x, y) (wy, (-wx))
            180 -> move is (x, y) ((-wx), (-wy))
            270 -> move is (x, y) ((-wy), wx)