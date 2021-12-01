main = do
--     let contents = "1721\n\
-- \979\n\
-- \366\n\
-- \299\n\
-- \675\n\
-- \1456"
    contents <- readFile "day01.in"  -- read in my assigned inputs

    let allLines = map (\x -> (read x :: Int)) $ lines contents  -- turn string into list of strings into list of integers

    -- print allLines  -- Could also be putStrLn $ show allLines
    print $ product [x | x <- allLines, (2020 - x) `elem` allLines]  -- runs on whole list, even after finding first match
