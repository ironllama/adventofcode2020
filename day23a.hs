import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

main = do
    let i = "389125467"
    -- let i = "156794823"
        i' = map digitToInt i

        -- x = goAround i' [i']
        x = goAround i' 100


    -- print i'
    -- mapM_ print $ snd x

    -- print $ fst x
    -- print . concat . map show . drop 1 . rotate $ fst x
    print x
    print . concat . map show . drop 1 $ rotate x

    -- print $ getn 3 [5, 4, 6, 8, 9, 6]

-- goAround :: [Int] -> [[Int]] -> ([Int], [[Int]])
-- goAround b x = let
goAround :: [Int] -> Int -> [Int]
goAround b x = let
    curr = head b
    b' = drop 1 b
    n3 = take 3 b'
    b'' = drop 3 b'
    -- subtract 1 from current and keep looking... if you get to lowest, then get highest
    n = getn (curr-1) b''
    ni = (fromJust $ n `elemIndex` b'') + 1
    nb = take ni b'' ++ n3 ++ drop ni b'' ++ [curr]
    -- in if length x == 10 then (nb, x) else goAround nb (x ++ [nb])
    in if x == 1 then nb else goAround nb (x-1)

getn :: Int -> [Int] -> Int
getn a xs
    | a `elem` xs = a
    | otherwise = if a < 1 then maximum xs else getn (a-1) xs

rotate :: [Int] -> [Int]
rotate a@(x:xs) = if x == 1 then a else rotate (xs++[x])

-- -- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
-- rotate :: Int -> [a] -> [a]
-- rotate _ [] = []
-- rotate n xs = zipWith const (drop n (cycle xs)) xs