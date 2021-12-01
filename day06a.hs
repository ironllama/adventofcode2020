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
        paras = joinPara $ lines input
        -- processed = map (removeSpaceAndDupes) paras
        -- numbers = map (\thisPara -> length thisPara) processed
        -- final = foldr (\thisNum accum -> accum + thisNum) 0 numbers
        -- final = sum numbers
        final = sum $ map (length . removeSpaceAndDupes) paras


    -- print $ lines input
    -- print paras
    -- print processed
    -- print numbers
    print final


removeSpaceAndDupes :: String -> String
removeSpaceAndDupes = foldr remove' []
    where remove' inChar inString
            | inChar == ' ' = inString
            | otherwise = if inChar `elem` inString then inString else inChar:inString

joinPara :: [String] -> [String]
joinPara = foldr joinHandler [[]]
    where joinHandler inLine inList@(x:xs)
            | inLine == "" = []:inList  -- if blank found, add a new list to the accumulator
            | otherwise = (inLine ++ x):xs  -- add to the first list in the accumulator