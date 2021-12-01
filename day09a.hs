main = do
--     let i = "35\n\
-- \20\n\
-- \15\n\
-- \25\n\
-- \47\n\
-- \40\n\
-- \62\n\
-- \55\n\
-- \65\n\
-- \95\n\
-- \102\n\
-- \117\n\
-- \150\n\
-- \182\n\
-- \127\n\
-- \219\n\
-- \299\n\
-- \277\n\
-- \309\n\
-- \576"
    i <- readFile "day09.in"

    let i' = map (\x -> read x::Int) $ lines i
        p = 25
        z = f p i'

    -- print i'
    print z

f :: Int -> [Int] -> Int
f p ys
    | p >= length ys = -1
    | otherwise = let
        xs = take p ys
        y = (drop p ys)!!0
        y' = foldr (\x' a -> if a || ((y - x') `elem` xs) then True else a) False xs
        in if y' then f p (drop 1 ys) else y
        -- in y