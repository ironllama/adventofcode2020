import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Maybe

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
--     let i = "class: 0-1 or 4-19\n\
-- \row: 0-5 or 8-19\n\
-- \seat: 0-13 or 16-19\n\
-- \\n\
-- \your ticket:\n\
-- \11,12,13\n\
-- \\n\
-- \nearby tickets:\n\
-- \3,9,18\n\
-- \15,1,5\n\
-- \5,14,9"
    i <- readFile "day16.in"

    let i' = splitOn [""] $ lines i
        cls = map (splitOn " or " . concat . tail . splitOn ": ") $ head i'
        cls' = map fcls $ cls  -- Create list of valid class values. Keep grouping by fields.
            where fcls y = concat $ foldr (\x a -> let
                                        rng = map (\y -> read y::Int) $ splitOn "-" x
                                        in [(head rng)..(last rng)]:a
                                    ) [] y
        tix = foldr (\x a -> (map (\y -> read y::Int) $ splitOn "," x):a) [] $ tail $ last i'
        tix' = foldr (\x a ->  -- Check ticket field values against valid class values. Keep grouping by fields
                    if foldr (\y b -> if b == True && y `S.member` (S.fromList $ concat cls') then True else False) True x
                        then x:a
                        else a
                ) [] tix
        -- pos = take (length cls') $ repeat $ S.singleton 0
        -- pos = take (length cls') $ repeat [0]
        -- pos' = foldr (\thisTix pos ->
        --             foldr (\x pos -> let
        --                 n = thisTix!!x
        --                 -- foldr (\z pos -> if y `elem` (cls'!!z) then (take z pos) ++ [S.insert (z + 1) (pos!!z)] ++ (drop (z + 1) pos) else pos) pos [0..(length pos - 1)]
        --                 in foldr (\z pos -> if n `elem` (cls'!!z) then (take z pos) ++ [(x + 1):(pos!!z)] ++ (drop (z + 1) pos) else pos) pos [0..(length pos - 1)]
        --                 -- foldr (\z pos -> if y `elem` (cls'!!z) then (take z pos) ++ [S.insert y (pos!!z)] ++ (drop (z + 1) pos) else pos) pos [0..(length pos - 1)]
        --                 -- foldr (\z pos -> let t = S.insert y (pos!!z) in pos) pos [0..(length pos - 1)]
        --             ) pos [0..(length pos - 1)]
        --         ) pos tix'

        pos' = map (\a -> map (!!a) tix') [0..(length cls' - 1)]  -- Get list of all first fields, second fields, etc. from tickets.
        pos'' = zip [0..] $ map (\a ->  -- Check those position fields against possible ticket fields. (ticket, [classes])
                    foldr (\b acc -> if a `intersect` (cls'!!b) == a then b:acc else acc) [] [0..(length cls' - 1)]
                    -- foldl' (\acc b -> if a `intersect` (cls'!!b) == a then acc ++ [b] else acc) [] [0..(length cls' - 1)]
                ) pos'
        ord = sortOn snd $ order pos'' []  -- Try to figure out which ticket fields correspond to which class fields. (ticket, [class])
        -- ord' = sort $ foldr (\x a -> if fst x < 6 then (head $ snd x):a else a) [] ord  -- Get ticket position of first six (departure) class fields.
        ord' = map (fst) $ take 6 ord
        you = map (\x -> read x::Int) . splitOn "," . last . head $ drop 1 i'  -- Read own ticket
        you' = map (you!!) ord'  -- Get corresponding fields from own ticket.

    -- print i'
    -- print cls
    -- print cls'
    -- print tix
    -- print tix'
    -- print pos
    -- print pos'
    print pos''
    print ord
    print ord'
    print you'
    print $ product you'

order work end = let  -- Use process of elimination. Each ticket field can be only one class field.
    lengths = map (\a -> length $ snd a) work
    i = fromMaybe (-1) $ 1 `elemIndex` lengths
    num = head $ snd (work!!i)
    in if i /= -1 then order (remove num $ (take i work) ++ (drop (i + 1) work)) ((work!!i):end) else end

remove :: Int -> [(Int, [Int])] -> [(Int, [Int])]
remove num zl = map (\a -> (fst a, num `delete` (snd a))) zl