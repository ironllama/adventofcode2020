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
    -- print $ count '#' $ process 0 3 allLines
    -- print $ count '#' $ process 0 0 1 1 allLines
    -- print $ count '#' $ process 0 0 3 1 allLines
    -- print $ count '#' $ process 0 0 5 1 allLines
    -- print $ count '#' $ process 0 0 7 1 allLines
    -- print $ count '#' $ process 0 0 1 2 allLines
    print $ product $ map (\x -> count '#' $ process 0 0 (x!!0) (x!!1) allLines) [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
    
-- process :: Int -> Int-> [String] -> String
-- process xPos xSkip [] = ""
-- process xPos xSkip (inItem:inList) = let
--     val = inItem !! (xPos `mod` (length inItem))
--     in [val] ++ process (xPos + xSkip) xSkip inList

process :: Int -> Int -> Int -> Int-> [String] -> String
process xPos yPos xSkip ySkip inList
    | yPos >= (length inList) = ""
    | otherwise = let
        inItem = inList !! yPos
        val = inItem !! (xPos `mod` (length inItem))
        in [val] ++ process (xPos + xSkip) (yPos + ySkip) xSkip ySkip inList

count :: (Eq a) => a -> [a] -> Int
count c = length . filter (==c)