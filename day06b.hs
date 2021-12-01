main = do
--     let input = "abc\n\
-- \\n\
-- \a\n\
-- \b\n\
-- \c\n\
-- \\n\
-- \ab\n\
-- \ac\n\
-- \\n\
-- \a\n\
-- \a\n\
-- \a\n\
-- \a\n\
-- \\n\
-- \b"
    input <- readFile "day06.in"

    let
        allInput = lines input
        indexes = map (removeSpaceAndDupes) $ joinPara allInput
        responses = joinPara' allInput
        goodResponses = map (everyoneHas) $ zip indexes responses
        -- countResponses = map (\thisResponse -> length $ filter (==True) thisResponse) goodResponses
        final = sum $ map (length. filter (==True)) goodResponses
        -- final = foldr (+) 0 countResponses
        -- final = sum countResponses

    -- print $ lines input
    -- print indexes
    -- print responses
    -- print work
    -- print goodResponses
    -- print countResponses
    print final

-- everyoneHas :: (String, [String]) -> [Bool]
everyoneHas thisWork = let
    theseIndexes = fst thisWork
    theseResponses = snd thisWork
    in map (\thisIndex ->
            foldr (\thisResponse accum ->
                if thisIndex `elem` thisResponse
                    then accum
                    else False
                ) True theseResponses
        ) theseIndexes

removeSpaceAndDupes :: String -> String
removeSpaceAndDupes = foldr remove' []
    where remove' inChar inString
            | inChar == ' ' = inString
            | otherwise = if inChar `elem` inString then inString else inChar:inString

-- Single string per grouping (grouping determined by blank new lines)
joinPara :: [String] -> [String]
joinPara = foldr joinHandler [[]]
    where joinHandler inLine inList@(x:xs)
            | inLine == "" = []:inList  -- if blank found, add a new list to the accumulator (string)
            | otherwise = (inLine ++ x):xs  -- add to the first list in the accumulator

-- Each grouping in separate list, and each group is a list of lines
joinPara' :: [String] -> [[String]]
joinPara' = foldr joinHandler [[]]
    where joinHandler inLine inList@(x:xs)
            | inLine == "" = []:inList  -- if blank found, add a new list to the accumulator (list)
            | otherwise = (inLine:x):xs  -- add to the first list in the accumulator
