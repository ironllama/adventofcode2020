import Data.List.Split (splitOn)
import qualified Data.Set as S

main = do
--     let i = "class: 1-3 or 5-7\n\
-- \row: 6-11 or 33-44\n\
-- \seat: 13-40 or 45-50\n\
-- \\n\
-- \your ticket:\n\
-- \7,1,14\n\
-- \\n\
-- \nearby tickets:\n\
-- \7,3,47\n\
-- \40,4,50\n\
-- \55,2,20\n\
-- \38,6,12"
    i <- readFile "day16.in"

    let i' = splitOn [""] $ lines i
        cls = concat . map (splitOn " or " . concat . tail . splitOn ": ") $ head i'
        cls' = S.fromList . concat $ foldr (\x a -> let  -- Create list of valid class values.
                    rng = map (\y -> read y::Int) $ splitOn "-" x
                    in [(head rng)..(last rng)]:a
                ) [] cls
        tix = concat $ foldr (\x a -> (map (\y -> read y::Int) $ splitOn "," x):a) [] (tail $ last i')
        tix' = foldr (\x a -> if x `S.member` cls' then a else x:a) [] tix  -- Check ticket field values against valid class values.

    -- print i'
    -- print cls'
    -- print tix'
    print $ sum tix'