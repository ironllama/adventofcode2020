main = do
--     let contents = "1721\n\
-- \979\n\
-- \366\n\
-- \299\n\
-- \675\n\
-- \1456"
    contents <- readFile "day01.in"

    let allLines = map (\x -> (read x :: Integer)) $ lines contents  -- turn string into list of strings into list of integers

    -- print allLines  -- Could also be putStrLn $ show allLines
    -- print [(x , y) | x <- allLines, y <- allLines, (2020 - x - y) `elem` allLines]  -- runs on whole list, even after finding first match
    -- print [x | x <- allLines, y <- allLines, (2020 - x - y) `elem` allLines]
    -- print $ firstNum allLines
    print . product . firstNum $ allLines

firstNum :: (Eq a, Num a) => [a] -> [a]
firstNum (x:xs) =
    case secondNum (2020 - x) xs of
        Nothing -> firstNum xs  -- recurse with new head/tail
        Just y -> x:y

secondNum :: (Eq a, Num a) => a -> [a] -> Maybe [a]
secondNum first_x (x:xs) =
    if (first_x - x) `elem` xs
        then Just [(first_x - x), x]
        else secondNum first_x xs
secondNum _ [] = Nothing  -- At the end of the list.
