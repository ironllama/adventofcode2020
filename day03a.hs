main = do
--     let input = "..##.......\n\
-- \#...#...#..\n\
-- \.#....#..#.\n\
-- \..#.#...#.#\n\
-- \.#...##..#.\n\
-- \..#.##.....\n\
-- \.#.#.#....#\n\
-- \.#........#\n\
-- \#.##...#...\n\
-- \#...##....#\n\
-- \.#..#...#.#"
    input <- readFile "day03.in"

    let allLines = lines input

    -- print $ length . filter (=='#') $ process 0 allLines
    print $ count '#' $ process 0 allLines

process :: Int -> [String] -> String
process xPos [] = ""
process xPos (inItem:inList) = let
    val = inItem !! (xPos `mod` (length inItem))
    in [val] ++ process (xPos + 3) inList

count :: (Eq a) => a -> [a] -> Int
count c = length . filter (==c)