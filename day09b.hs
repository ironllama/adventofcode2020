import Data.Maybe
import Data.List

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
        e = 1038347917  -- answer from part a
        -- e = 127  -- answer from part a
        s = fromMaybe (-1) $ elemIndex e i'
        i'' = take s i'
        -- z = f e i'' 0 (False, (0,0))
        y = foldr
            (\x a ->
                if tFst a == False
                    then f e (take x i'') 0 (False, (0, 0), (0, 0))
                    else a
            )
            (False, (0, 0), (0, 0))
            [0..s-1]  -- make sure working list is backwords on a foldr

        z = if tFst y == True then (fst $ tSnd y) + (snd $ tSnd y) else -1

    -- print i'
    -- print i''
    print y
    print z

f :: Int -> [Int] -> Int -> (Bool, (Int, Int), (Int, Int)) -> (Bool, (Int, Int), (Int, Int))
f _ [] _ t = (False, tSnd t, tTrd t)  -- ran out of numbers before reaching goal
f e ys a t
    | a > e = (False, tSnd t, tTrd t)  -- range got too high
    -- | a > e = (False, tSnd t, (-3, -3))
    | a == e = (True, tSnd t, tTrd t)  -- found!
    -- | a == e = (True, tSnd t, (-2, -2))
    | otherwise = let
        x = last ys
        a' = x + a
        l = fst $ tSnd t
        g = snd $ tSnd t
        in f e (init ys) a' (
                False,  -- not found, yet
                (if l == 0 || x < l then x else l, if g == 0 || x > g then x else g),  -- keeping track of least and greatest
                (if (fst $ tTrd t) == 0 then x else fst $ tTrd t, x)  -- keeping track of range
                -- (if (fst $ tTrd t) == 0 then x else fst $ tTrd t, a')
            )
        -- in [x, a', l, m]

-- No built-ins for triples, so have to make own versions of accessors.
tFst :: (a, b, c) -> a
tFst (x, _, _) = x

tSnd :: (a, b, c) -> b
tSnd (_, x, _) = x

tTrd :: (a, b, c) -> c
tTrd (_, _, x) = x
