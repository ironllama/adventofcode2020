import Data.List (foldl')

main = do
--     let input = "FBFBBFFRLR\n\
-- \BFFFBBFRRR\n\
-- \FFFBBBFRRR\n\
-- \BBFFBBFRLL"
    input <- readFile "day05.in"

    let allInput = lines input
        sortedInput = qsort allInput
        highest7 = take 7 $ sortedInput !! 0
        allHigh7 = filter (\inItem -> take 7 inItem == highest7) sortedInput
        highest = (reverse allHigh7) !! 0
        translated = map repl highest
        final = binStringToInt translated

    -- print sortedInput
    -- print highest7
    -- print allHigh7
    -- print highest
    -- print translated
    print final

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
