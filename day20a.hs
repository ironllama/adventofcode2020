-- import Data.List (foldl')

main = do
--     let i ="Tile 2311:\n\
-- \..##.#..#.\n\
-- \##..#.....\n\
-- \#...##..#.\n\
-- \####.#...#\n\
-- \##.##.###.\n\
-- \##...#.###\n\
-- \.#.#.#..##\n\
-- \..#....#..\n\
-- \###...#.#.\n\
-- \..###..###\n\
-- \\n\
-- \Tile 1951:\n\
-- \#.##...##.\n\
-- \#.####...#\n\
-- \.....#..##\n\
-- \#...######\n\
-- \.##.#....#\n\
-- \.###.#####\n\
-- \###.##.##.\n\
-- \.###....#.\n\
-- \..#.#..#.#\n\
-- \#...##.#..\n\
-- \\n\
-- \Tile 1171:\n\
-- \####...##.\n\
-- \#..##.#..#\n\
-- \##.#..#.#.\n\
-- \.###.####.\n\
-- \..###.####\n\
-- \.##....##.\n\
-- \.#...####.\n\
-- \#.##.####.\n\
-- \####..#...\n\
-- \.....##...\n\
-- \\n\
-- \Tile 1427:\n\
-- \###.##.#..\n\
-- \.#..#.##..\n\
-- \.#.##.#..#\n\
-- \#.#.#.##.#\n\
-- \....#...##\n\
-- \...##..##.\n\
-- \...#.#####\n\
-- \.#.####.#.\n\
-- \..#..###.#\n\
-- \..##.#..#.\n\
-- \\n\
-- \Tile 1489:\n\
-- \##.#.#....\n\
-- \..##...#..\n\
-- \.##..##...\n\
-- \..#...#...\n\
-- \#####...#.\n\
-- \#..#.#.#.#\n\
-- \...#.#.#..\n\
-- \##.#...##.\n\
-- \..##.##.##\n\
-- \###.##.#..\n\
-- \\n\
-- \Tile 2473:\n\
-- \#....####.\n\
-- \#..#.##...\n\
-- \#.##..#...\n\
-- \######.#.#\n\
-- \.#...#.#.#\n\
-- \.#########\n\
-- \.###.#..#.\n\
-- \########.#\n\
-- \##...##.#.\n\
-- \..###.#.#.\n\
-- \\n\
-- \Tile 2971:\n\
-- \..#.#....#\n\
-- \#...###...\n\
-- \#.#.###...\n\
-- \##.##..#..\n\
-- \.#####..##\n\
-- \.#..####.#\n\
-- \#..#.#..#.\n\
-- \..####.###\n\
-- \..#.#.###.\n\
-- \...#.#.#.#\n\
-- \\n\
-- \Tile 2729:\n\
-- \...#.#.#.#\n\
-- \####.#....\n\
-- \..#.#.....\n\
-- \....#..#.#\n\
-- \.##..##.#.\n\
-- \.#.####...\n\
-- \####.#.#..\n\
-- \##.####...\n\
-- \##..#.##..\n\
-- \#.##...##.\n\
-- \\n\
-- \Tile 3079:\n\
-- \#.#.#####.\n\
-- \.#..######\n\
-- \..#.......\n\
-- \######....\n\
-- \####.#..#.\n\
-- \.#...#.##.\n\
-- \#.#####.##\n\
-- \..#.###...\n\
-- \..#.......\n\
-- \..#.###..."
    i <- readFile "day20.in"

    let i' = foldr f [(0, [])] $ lines i
                where f x a@(y:ys)
                        | x == "" = (0, []):a
                        | head x == 'T' = (read (init . last $ words x)::Int, snd y):ys
                        | otherwise = (fst y, x:snd y):ys

        ns = map (\tile -> let
                    tile' = snd tile
                    t = head tile'
                    b = last tile'
                    l = map (\x -> head x) tile'
                    r = map (\x -> last x) tile'
                    in (fst tile, toBinNum $ [t, b, l, r, reverse t, reverse b, reverse l, reverse r])
                ) i'

        -- matches = map (\tile -> (fst tile, foldr f 0 $ snd tile)
        --                 where f n a = a + (length $ getMatchTile (fst tile) n ns)  -- Doesn't like where inside the map?
        --             ) ns
        matches = map (\tile -> let f n a = a + (length $ getMatchTile (fst tile) n ns)
                                in (fst tile, foldr f 0 $ snd tile)
                    ) ns

        corners = filter (\a -> snd a == 4) matches

    -- print i'
    -- print ns
    print corners
    print $ product $ map (\a -> fst a) corners

toBinNum :: [String] -> [Int]
toBinNum = map (\x -> binStringToInt $ map (\y -> if y == '#' then '1' else '0') x)

binStringToInt :: String -> Int
-- binStringToInt = foldl' (\accum x -> (accum * 2) + (charToInt x)) 0  -- Using foldl' instead of foldl for performance
binStringToInt = foldr (\x accum -> (accum * 2) + (if x == '0' then 0 else 1)) 0 . reverse -- Using foldl' instead of foldl for performance

getMatchTile :: Int -> Int -> [(Int, [Int])] -> [(Int, [Int])]
getMatchTile i n = foldr (\x a -> if i /= fst x && n `elem` snd x then x:a else a) []