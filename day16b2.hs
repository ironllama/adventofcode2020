import Data.List
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.Maybe

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Ix

main = do
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

        -- pos' = map (\a -> map (!!a) tix') [0..(length cls' - 1)]  -- Get list of all first fields, second fields, etc.
        -- pos'' = zip [0..] $ map (\a ->  -- Check those position fields against possible ticket fields
        --             foldr (\b acc -> if a `intersect` (cls'!!b) == a then b:acc else acc) [] [0..(length cls' - 1)]
        --         ) pos'
        -- ord = sortOn fst $ order pos'' []  -- Try to figure out which ticket fields correspond to which class fields.
        -- -- ord' = sort $ foldr (\x a -> if fst x < 6 then (head $ snd x):a else a) [] ord  -- Get ticket position of first six (departure) class fields.
        -- ord' = map (head . snd) $ take 6 ord

        you = map (\x -> read x::Int) . splitOn "," . last . head $ drop 1 i'  -- Read own ticket
        -- you' = map (you!!) ord'  -- Get corresponding fields from own ticket.

    -- print i'
    -- print cls
    -- print cls'
    -- print tix
    -- print tix'
    -- print pos
    -- print pos'
    -- print pos''
    -- print ord
    -- print ord'
    -- print you'
    -- print $ product you'
    print $ part2 tix' you


-- order work end = let  -- Use process of elimination. Each ticket field can be only one class field.
--     lengths = map (\a -> length $ snd a) work
--     i = fromMaybe (-1) $ 1 `elemIndex` lengths
--     num = head $ snd (work!!i)
--     in if i /= -1 then order (remove num $ (take i work) ++ (drop (i + 1) work)) ((work!!i):end) else end

-- remove :: Int -> [(Int, [Int])] -> [(Int, [Int])]
-- remove num zl = map (\a -> (fst a, num `delete` (snd a))) zl

type Range = (Int, Int)

cR :: Range -> Range -> (Int -> Bool)
cR (l1, h1) (l2, h2) n = inRange (l1, h1) n || inRange (l2, h2) n

ranges =
  [ (34, 724) `cR` (735, 974),
    (40, 521) `cR` (534, 950),
    (40, 329) `cR` (353, 973),
    (37, 258) `cR` (268, 964),
    (32, 650) `cR` (665, 964),
    (39, 373) `cR` (398, 950),
    (42, 431) `cR` (447, 952),
    (36, 536) `cR` (552, 972),
    (45, 666) `cR` (678, 952),
    (49, 836) `cR` (852, 952),
    (35, 600) `cR` (623, 953),
    (50, 920) `cR` (929, 950),
    (35, 853) `cR` (870, 973),
    (34, 309) `cR` (318, 965),
    (42, 267) `cR` (292, 962),
    (46, 632) `cR` (642, 954),
    (47, 746) `cR` (754, 960),
    (32, 406) `cR` (423, 963),
    (37, 797) `cR` (810, 973),
    (35, 766) `cR` (784, 952)
  ]

rangesA = zip [0 ..] ranges

-- part2 :: [[Int]] -> [Int] -> Maybe Int
part2 validTickets myTicket = foo
  where
    constraints = foldl1' intersect <$> transpose (map aaaa <$> validTickets)
    init = head . concat . filter ((== 1) . length) $ constraints
    foo = do (_, (answer', _)) <- iterate (>>= backsol) (pure (init, (constraints, IS.singleton init))) !! 19
             let answer = concat answer'
             l <- sequenceA $ (`elemIndex` answer) <$> [0 .. 5]
            --  sequenceA $ (`elemIndex` answer) <$> [0 .. 5]  -- To print out the matching indexes
             pure (product ((myTicket !!) <$> l))

backsol :: (Int, ([[Int]], IntSet)) -> Maybe (Int, ([[Int]], IntSet))
backsol (n, (l, solset)) = (,) . fst <$> IS.minView (sols IS.\\ solset) <*> pure (l', sols)
  where
    sols = IS.fromList . concat . filter ((== 1) . length) $ l'
    l' = map f l
    f l = if length l == 1 then l else filter (/= n) l

aaaa :: Int -> [Int]
aaaa x = [n | (n, f) <- rangesA, f x]
