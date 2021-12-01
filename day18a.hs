import Data.List
import Data.Char (digitToInt)

main = do
    -- let i = "1 + 2 * 3 + 4 * 5 + 6"
--     let i = "1 + 2 * 3 + 4 * 5 + 6\n\
-- \1 + (2 * 3) + (4 * (5 + 6))\n\
-- \2 * 3 + (4 * 5)\n\
-- \5 + (8 * 3 + 9 + 3 * 4 * 3)\n\
-- \5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))\n\
-- \((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    i <- readFile "day18.in"

    -- print $ parens "4 * 5) + 5" [] 0
    -- print $ parens "5 + (8 * 3 + 9 + 3 * 4 * 3)" [] 0
    print . sum . map (\a -> process a ' ' 0) $ lines i

process :: String -> Char -> Int -> Int
process [] _ acc = acc
process (x:xs) op acc
    | x == ' ' = process xs op acc
    | x == '+' || x == '*' = process xs x acc
    | x == '(' = let p' = parens xs [] 0 in process (snd p') ' ' $ oper op (fst p') acc
    | otherwise = process xs ' ' $ oper op (digitToInt x) acc
    -- | otherwise = case op of
    --     '+' -> process xs ' ' $ acc + (digitToInt x)
    --     '*' -> process xs ' ' $ acc * (digitToInt x)
    --     _ -> process xs ' ' $ acc + (digitToInt x)  -- Should only be run at start?

oper :: Char -> Int -> Int -> Int
oper op x acc
    | op == '+' = acc + x
    | op == '*' = acc * x
    | otherwise = x

parens :: String -> String -> Int -> (Int, String)
parens [] acc _ = (0, acc)
parens (x:xs) acc depth
    | x == ')' = if depth == 0 then (process acc ' ' 0, xs)
                    else parens xs (acc ++ [x]) (depth - 1)
    | x == '(' = parens xs (acc ++ [x]) (depth + 1)
    | otherwise = parens xs (acc ++ [x]) depth