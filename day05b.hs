import Data.List (foldl')

main = do
--     let input = "FBFBBFFLLL\n\
-- \FBFBBFFRLR\n\
-- \FBFBBFFLRL\n\
-- \FBFBBFFRLL\n\
-- \FBFBBFFLLR"
    input <- readFile "day05.in"

    let allInput = lines input
        sortedInput = qsort $ map passToInt allInput
        begin = head sortedInput
        end = last sortedInput
        expected = [x | x <- [begin .. end]]
        final = foldr (\thisPass accum -> filter (/=(passToInt thisPass)) accum) expected allInput

    -- print sortedInput
    -- print begin
    -- print end
    -- print expected
    print final


passToInt :: String -> Int
passToInt thisPass = binStringToInt $ map repl thisPass

repl :: Char -> Char
repl 'F' = '0'
repl 'B' = '1'
repl 'L' = '0'
repl 'R' = '1'

binStringToInt :: String -> Int
binStringToInt = foldl' (\accum x -> (accum * 2) + (charToInt x)) 0  -- Using foldl' instead of foldl for performance

charToInt :: Char -> Int
charToInt x = if x == '0' then 0 else 1

-- A sorting function can also be imported from Data.List, but for fun, here is a quicksort
-- From https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort lesser) ++ [x] ++ (qsort greater) where
    lesser = filter (< x) xs
    greater = filter (>= x) xs
