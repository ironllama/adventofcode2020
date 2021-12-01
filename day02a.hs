-- import Data.List.Split
import Data.List
import Data.Char

main = do
--     let input = "1-3 a: abcde\n\
-- \1-3 b: cdefg\n\
-- \2-9 c: ccccccccc"
    input <- readFile "day02.in" 
    
    let allLines = lines input
        -- codePass = splitBy ": " allLines
        -- codePass = words allLines
    
    -- print codePass
    -- print $ map (\x -> words x) allLines
    -- print $ [splitBy ':' x | x <- allLines]
    -- print [splitBy ':' x | x <- allLines]
    -- let chopped = map chopup allLines
    print $ length $ filter (==True) $ map (process . chopup) allLines

process :: [String] -> Bool
process inStrList = let
    min = read (inStrList !! 0) :: Int
    max = read (inStrList !! 1) :: Int
    needle = (inStrList !! 2) !! 0
    haystack = inStrList !! 3
    numOccur = length $ filter (\x -> x == needle) haystack
    in numOccur >= min && numOccur <= max

chopup :: String -> [String]
chopup inStr = let
    tokens = words inStr
    occur = splitBy '-' (tokens !! 0)
    needle = (tokens !! 1) !! 0
    -- in occur ++ [needle] ++ tokens !! 2
    -- in tokens
    in occur ++ [[needle]] ++ [tokens !! 2]
    -- return True


trim = dropWhileEnd isSpace . dropWhile isSpace

-- From https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l | otherwise = (c:x):xs

-- splitBy :: Char -> String -> [String]
-- splitBy _ "" = [];
-- splitBy delimiterChar inputString = foldr f [""] inputString
--   where f :: Char -> [String] -> [String]
--         f currentChar allStrings@(partialString:handledStrings)
--           | currentChar == delimiterChar = "":allStrings -- start a new partial string at the head of the list of all strings
--           | otherwise = (currentChar:partialString):handledStrings -- add the current char to the partial string
