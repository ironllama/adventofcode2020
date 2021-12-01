main = do
--     let input = "1-3 a: abcde\n\
-- \1-3 b: cdefg\n\
-- \2-9 c: ccccccccc"
    input <- readFile "day02.in" 
    
    let allLines = lines input
    -- print $ map (process . chopup) allLines
    print $ length $ filter (==True) $ map (process . chopup) allLines


xor :: Bool -> Bool -> Bool
xor a b = a /= b

process :: [String] -> Bool
process inStrList =
    let first = read (inStrList !! 0) :: Int
        second = read (inStrList !! 1) :: Int
        needle = (inStrList !! 2) !! 0
        haystack = inStrList !! 3
        firstGood = haystack !! (first - 1) == needle
        secondGood = haystack !! (second - 1) == needle
    in xor firstGood secondGood

chopup :: String -> [String]
chopup inStr =
    let tokens = words inStr
        occur = splitBy '-' (tokens !! 0)
        needle = (tokens !! 1) !! 0
    in occur ++ [[needle]] ++ [tokens !! 2]

-- From https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l | otherwise = (c:x):xs