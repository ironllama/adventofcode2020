import Data.List
import Data.Ord (comparing)
-- import qualified Data.Sequence as S
-- import Data.Maybe (fromJust)

main = do
--     let i = "sesenwnenenewseeswwswswwnenewsewsw\n\
-- \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
-- \seswneswswsenwwnwse\n\
-- \nwnwneseeswswnenewneswwnewseswneseene\n\
-- \swweswneswnenwsewnwneneseenw\n\
-- \eesenwseswswnenwswnwnwsewwnwsene\n\
-- \sewnenenenesenwsewnenwwwse\n\
-- \wenwwweseeeweswwwnwwe\n\
-- \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
-- \neeswseenwwswnwswswnw\n\
-- \nenwswwsewswnenenewsenwsenwnesesenew\n\
-- \enewnwewneswsewnwswenweswnenwsenwsw\n\
-- \sweneswneswneneenwnewenewwneswswnese\n\
-- \swwesenesewenwneswnwwneseswwne\n\
-- \enesenwswwswneneswsenwnewswseenwsese\n\
-- \wnwnesenesenenwwnenwsewesewsesesew\n\
-- \nenewswnwewswnenesenwnesewesw\n\
-- \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
-- \neswnwewnwnwseenwseesewsenwsweewe\n\
-- \wseweeenwnesenwwwswnew"
    i <- readFile "day24.in"

    let i' = lines i
        p = foldr (\v a -> let n = initTiles v ' ' (0,0) in if n `elem` a then delete n a else n:a) [] i'
        -- mnx = fst $ minimumBy (comparing fst) p
        -- mxx = fst $ maximumBy (comparing fst) p
        -- mny = snd $ minimumBy (comparing snd) p
        -- mxy = snd $ maximumBy (comparing snd) p
        -- mn = (mnx, mny)
        -- mx = (mxx, mxy)
        -- z1 = day mnx mxx mny mxy p
        -- z = day mnx mxx mny mxy z1

    -- print $ initTiles i ' ' (0, 0)
    -- print $ length i'
    -- print p
    -- print $ length p
    -- print $ "Min: " ++ (show mn)
    -- print $ "Max: " ++ (show mx)
    -- print z
    -- print $ length z
    -- print $ length $ foldr (\x a -> day a) p [1..10]  -- (1.13 secs, 87,860,752 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..20]  -- (4.33 secs, 184,294,184 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..30]  -- (10.05 secs, 317,097,824 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..40]  -- (17.39 secs, 491,097,448 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..50]  -- (35.76 secs, 714,883,128 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..60]  -- (79.66 secs, 990,915,936 bytes)
    print $ length $ foldr (\x a -> day a) p [1..100]  -- (347.88 secs, 2,731,028,488 bytes)

    -- Reusing, instead of starting a new list each day.
    -- print $ length $ foldr (\x a -> day a) p [1..30]  -- (12.40 secs, 695,899,504 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..40]  -- (29.93 secs, 1,234,841,424 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..50]  -- (50.79 secs, 2,044,458,384 bytes)
    -- print $ length $ foldr (\x a -> day a) p [1..100]  -- (414.86 secs, 13,905,384,800 bytes)

    -- print $ length $ fst $ foldr (\x a -> day a) (p, [mnx, mxx, mny, mxy]) [1..20]  -- (4.01 secs, 196,533,888 bytes
    -- print $ length $ fst $ foldr (\x a -> day a) (p, [mnx, mxx, mny, mxy]) [1..30]  -- (11.99 secs, 338,701,312 bytes)
    -- print $ length $ fst $ foldr (\x a -> day a) (p, [mnx, mxx, mny, mxy]) [1..50]  -- (36.03 secs, 765,894,936 bytes)

    -- print $ length $ foldr (\x a -> day a ) (S.fromList p) [1..20]  -- (13.41 secs, 8,108,830,384 bytes)
    -- print $ length $ foldr (\x a -> day a ) (S.fromList p) [1..30]  -- (35.90 secs, 18,234,652,640 bytes)
    -- print $ length $ foldr (\x a -> day a ) (S.fromList p) [1..40]  -- (67.78 secs, 35,591,322,224 bytes)
    -- print $ length $ foldr (\x a -> day a ) (S.fromList p) [1..50]  -- (119.54 secs, 63,724,820,888 bytes


day :: [(Int, Int)] -> [(Int, Int)]
day tiles = let
    mnx = fst $ minimumBy (comparing fst) tiles
    mxx = fst $ maximumBy (comparing fst) tiles
    mny = snd $ minimumBy (comparing snd) tiles
    mxy = snd $ maximumBy (comparing snd) tiles
    in foldr (\y a ->
            foldr (\x a ->
                if doMe (x, y) tiles == 1 then (x,y):a else a
            ) a [(mnx - 1)..(mxx + 1)]
        ) [] [(mny - 1)..(mxy + 1)]
    -- in foldr (\y a ->
    --         foldr (\x a ->
    --             if doMe (x, y) tiles == 1 then (if (x,y) `elem` a then a else (x,y):a)
    --                 else if (x,y) `elem` a then delete (x,y) a else a
    --         ) a [(mnx - 1)..(mxx + 1)]
    --     ) tiles [(mny - 1)..(mxy + 1)]

-- day :: ([(Int, Int)], [Int]) -> ([(Int, Int)], [Int])
-- day tiles = let
--     mnx = (snd tiles)!!0
--     mxx = (snd tiles)!!1
--     mny = (snd tiles)!!2
--     mxy = (snd tiles)!!3
--     in foldr (\y a ->
--             foldr (\x a ->
--                 if doMe (x, y) (fst tiles) == 1 then ((x,y):(fst a), bounds (x,y) (snd a)) else a
--             ) a [(mnx - 1)..(mxx + 1)]
--         ) ([],(snd tiles)) [(mny - 1)..(mxy + 1)]

-- bounds :: (Int, Int) -> [Int] -> [Int]
-- bounds (x,y) minmax = let
--     minX' = if x < minmax!!0 then x else minmax!!0
--     maxX' = if x > minmax!!1 then x else minmax!!1
--     minY' = if y < minmax!!2 then y else minmax!!2
--     maxY' = if y > minmax!!3 then y else minmax!!3
--     in [minX', maxX', minY', maxY']


doMe :: (Int, Int) -> [(Int, Int)] -> Int
doMe (x,y) blks = let
    curr = isBlk (x, y) blks
    eveny = y `mod` 2
    ne = isBlk (x+eveny, y+1) blks
    e = isBlk (x+1, y) blks
    se = isBlk (x+eveny, y-1) blks
    sw = isBlk (x-(1-eveny), y-1) blks
    w = isBlk (x-1, y) blks
    nw = isBlk (x-(1-eveny), y+1) blks
    score = ne + e + se + sw + w + nw
    in if curr == 1 then (if score == 0 || score > 2 then 0 else 1)  -- If black, then if 0 black neighbors or more than 2, then flip to white
        else (if score == 2 then 1 else 0)  -- If white, then if exactly 2 black neighbors, then flip to black

isBlk :: (Int, Int) -> [(Int, Int)] -> Int
isBlk (x,y) blks = if (x,y) `elem` blks then 1 else 0


-- day :: S.Seq (Int, Int) -> S.Seq (Int, Int)
-- day tiles = let
--     minmax = bounds tiles
--     mnx = minmax!!0
--     mxx = minmax!!1
--     mny = minmax!!2
--     mxy = minmax!!3
--     in foldr (\y a ->
--             foldr (\x a ->
--                 -- if doMe (x, y) (fst tiles) == 1 then (M.(fst a), bounds (x,y) (snd a)) else a
--                 if doMe (x, y) tiles == 1 then (if (x,y) `S.elemIndexL` a /= Nothing then a else (x,y) S.<| a)
--                     else if (x,y) `S.elemIndexL` a /= Nothing then S.deleteAt (fromJust $ S.elemIndexL (x,y) a) a else a
--             ) a [(mnx - 1)..(mxx + 1)]
--         ) tiles [(mny - 1)..(mxy + 1)]

-- bounds :: S.Seq (Int, Int) -> [Int] 
-- bounds xs = foldr (\v a -> let
--         x = fst v
--         y = snd v
--         minX' = if x < a!!0 then x else a!!0
--         maxX' = if x > a!!1 then x else a!!1
--         minY' = if y < a!!2 then y else a!!2
--         maxY' = if y > a!!3 then y else a!!3
--         in [minX', maxX', minY', maxY']
--     ) [0, 0, 0, 0] xs

-- doMe :: (Int, Int) -> S.Seq (Int, Int) -> Int
-- doMe (x,y) blks = let
--     curr = isBlk (x, y) blks
--     eveny = y `mod` 2
--     ne = isBlk (x+eveny, y+1) blks
--     e = isBlk (x+1, y) blks
--     se = isBlk (x+eveny, y-1) blks
--     sw = isBlk (x-(1-eveny), y-1) blks
--     w = isBlk (x-1, y) blks
--     nw = isBlk (x-(1-eveny), y+1) blks
--     score = ne + e + se + sw + w + nw
--     in if curr == 1 then (if score == 0 || score > 2 then 0 else 1)  -- If black, then if 0 black neighbors or more than 2, then flip to white
--         else (if score == 2 then 1 else 0)  -- If white, then if exactly 2 black neighbors, then flip to black

-- isBlk :: (Int, Int) -> S.Seq (Int, Int) -> Int
-- isBlk (x,y) blks = if (x,y) `S.elemIndexL` blks /= Nothing then 1 else 0


initTiles :: String -> Char -> (Int, Int) -> (Int, Int)
initTiles [] _ a = a
initTiles (x:xs) p a = if x == 'n' || x == 's' then initTiles xs x a
                        else let
                            eveny = (snd a) `mod` 2 == 0
                            ew = if eveny && (p == 'n' || p == 's') then (if x == 'e' then 0 else -1)
                                    else if (p == 'n' || p == 's') then (if x == 'e' then 1 else 0)
                                    else if x == 'e' then 1 else -1
                            ns = case p of
                                    'n' -> 1
                                    's' -> -1
                                    _ -> 0
                            in initTiles xs ' ' ((fst a) + ew, (snd a) + ns)
