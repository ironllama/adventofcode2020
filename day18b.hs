import Data.List (foldl')
import Data.List.Split (splitOn)
-- import Data.Char (digitToInt)

main = do
    -- let i = "3 + 1 + 2 * 5 + 2 * 3 + 4 * 5 * 5 + 6 * 2 * 3 + 5 + 2 + 1"
    -- let i = "2 * 5"
    -- let i = "1 + 2 * 3 + 4 * 5 + 6"
--     let i = "1 + 2 * 3 + 4 * 5 + 6\n\
-- \1 + (2 * 3) + (4 * (5 + 6))\n\
-- \2 * 3 + (4 * 5)"
--     let i = "1 + 2 * 3 + 4 * 5 + 6\n\
-- \1 + (2 * 3) + (4 * (5 + 6))\n\
-- \2 * 3 + (4 * 5)\n\
-- \5 + (8 * 3 + 9 + 3 * 4 * 3)\n\
-- \5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))\n\
-- \((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    i <- readFile "day18.in"

    -- print $ parens "4 * 5) + 5" [] 0
    -- print $ parens "5 + (8 * 3 + 9 + 3 * 4 * 3)" [] 0
        -- added = add (splitOn "+" i) (-1) []
        -- multed = map (\x -> read x::Int) $ splitOn "*" added
        -- multed = product $ map (\x -> read x::Int) $ splitOn "*" added
    -- let z = map (\a -> process a ' ' 0) $ lines i
    let z = map (\i' -> process $ go i') $ lines i

    -- print $ lines i
    -- print z
    -- print $ sum z
    -- foldr (\x a -> ) [] [0..(length multList - 1)]
    -- print $ process i
    -- print $ map (\x -> parens x []) $ lines i
    -- print z
    print $ sum z

go :: String -> String
go i = if fst back /= -1 then go (snd back) else snd back
        where back = go' i

go' :: String -> (Int, String)
go' i' = if '(' `elem` i'
            then foldl' (\a x -> 
                    if snd a == [] then case (i'!!x) of
                        '(' -> (x, [])
                        ')' -> (x, take (fst a) i' ++ (show $ process (drop ((fst a) + 1) $ take x i')) ++ drop (x + 1) i')
                        _ -> a
                    else a
                ) (-1, []) [0..length i' - 1]
            else (-1, i')

process :: String -> Int
process "" = 0
process x = product $ map (\x -> read x::Int) $ splitOn "*" $ add (splitOn "+" x) (-1) []

-- addList = map (\x -> read x::Int) $ splitOn "+" multList
add :: [String] -> Int -> String -> String
-- add [x] end acc = let
--                     a = words x
--                     new = acc ++ (show $ end + (read (head a)::Int))
--                     in if length a > 1 then new ++ (unwords $ tail a) else new
-- add [""] end acc = []
add [] end acc = acc ++ (show end)
add (x:xs) end acc = let
    a = words x
    aBeg = read (head a)::Int
    aEnd = read (last a)::Int 
    end' = if length a > 1 then aEnd
            else if end == -1 then aEnd
            else end + aEnd
    new = if length a > 1
            then if end == -1 then unwords $ init a
                else acc ++ (show $ end + aBeg) ++ (unwords . tail $ init a)
            else if end == -1 then []
                else acc
    in add xs end' new 

-- parens :: String -> String -> String
-- parens [] acc = if length acc > 0 then show $ process acc else acc
-- parens (x:xs) acc
--     | x == '(' = acc ++ parens (xs ++ []) []
--     | x == ')' = if length acc > 0 then (show $ process acc) else parens xs []
--     | otherwise = parens xs (acc ++ [x])
    -- foldl' (\a x -> let
    --         c = i!!x
    --         in if snd a == []
    --             then case c of
    --                 '(' -> (x, [])
    --                 ')' -> (x, take (fst a) i ++ show $ process (drop ((fst a) + 1) $ take x i) ++ drop (x + 1) i)
    --                 _ -> a
    --             else a
    --     ) (-1, []) [0..length i - 1]



-- mult [] acc = acc
-- mult (x:xs) left op acc
--     | x == ' ' || x == '+' = mult xs left op acc
--     | x == '*' = mult xs left '*' acc
--     | otherwise = case op of
--         '*' -> if left /= -1 then else mult xs x op acc
--         _ -> mult xs x op acc

-- mult xs (x * acc)

-- add [] acc = acc
-- add (x:xs) acc = add xs (x + acc)


-- process :: String -> Char -> Int -> Int
-- process [] _ acc = acc
-- process (x:xs) op acc
--     | x == ' ' = process xs op acc
--     | x == '+' || x == '*' = process xs x acc
--     | x == '(' = let p' = parens xs [] 0 in process (snd p') ' ' $ oper op (fst p') acc
--     | otherwise = process xs ' ' $ oper op (digitToInt x) acc
    -- | otherwise = case op of
    --     '+' -> process xs ' ' $ acc + (digitToInt x)
    --     '*' -> process xs ' ' $ acc * (digitToInt x)
    --     _ -> process xs ' ' $ acc + (digitToInt x)  -- Should only be run at start?

-- oper :: Char -> Int -> Int -> Int
-- oper op x acc
--     | op == '+' = acc + x
--     | op == '*' = acc * x
--     | otherwise = x

-- parens :: String -> String -> Int -> (Int, String)
-- parens [] acc depth = if depth == 0 then process acc ' ' 0(0, acc)
-- parens (x:xs) acc depth
--     | x == ')' = if depth == 1 then (process acc ' ' 0, xs)
--                     else parens xs (acc ++ [x]) (depth - 1)
--     | x == '(' = parens xs (acc ++ [x]) (depth + 1)
--     | otherwise = parens xs (acc ++ [x]) depth