import Data.Char (digitToInt)
import Data.List (elemIndex, foldl')
import Data.Maybe (fromJust)
import qualified Data.Sequence as S
-- import Control.Monad (foldM)
-- import Control.Monad.ST
-- import qualified Data.Vector.Unboxed.Mutable as V
-- import qualified Data.Vector.Unboxed as U

main = do
    -- let i = "389125467"
    let i = "156794823"
        -- i' = (map digitToInt i)
        -- i' = (map digitToInt i) ++ [10..1000000]
        -- i' = S.fromList $ (map digitToInt i) ++ [10..1000000]
        -- i' = S.fromList ((map digitToInt i) ++ [10..100000])  -- (16.65 secs, 426,057,208 bytes)
        -- i' = S.fromList ((map digitToInt i) ++ [10..10000])  -- (1.33 secs, 36,026,512 bytes)
        -- i' = S.fromList ((map digitToInt i) ++ [10..1000]) -- (0.10 secs, 3,214,920 bytes)
        -- i' = S.fromList ((map digitToInt i) ++ [10..100])
        -- i' = S.fromList ((map digitToInt i) ++ [10..70])
        -- i' = S.fromList $ map digitToInt i

        -- x = goAround i' [i']
        -- x = goAround i' 10000000
        -- x = goAround i' 1000000  -- w/ 10k list, (1397.34 secs, 850,928,729,688 bytes)
        -- x = goAround i' 100000  -- w/ 10k list, (167.93 secs, 86,345,747,344 bytes) vs seq (57.47 secs, 42,870,659,912 bytes)
        -- x = goAround i' 10000  -- w/ 10k list, (14.92 secs, 10,228,446,968 bytes) vs seq (7.06 secs, 6,004,341,968 bytes)
        -- x = goAround i' 1000  -- w/ 10k list, (1.26 secs, 1,068,066,856 bytes) vs seq (0.64 secs, 673,733,424 bytes)

        -- x = goAround i' 1000  -- w/ 1m seq, (91.05 secs, 66,808,851,488 bytes)
        -- x = goAround i' 100  -- w/ 1m seq, (7.82 secs, 6,803,752,808 bytes)
        -- x = goAround i' 10  -- w/ 1m seq, (0.73 secs, 803,366,728 bytes)
        -- x = goAround i' 1  -- w/ 1m seq, (0.06 secs, 103,333,952 bytes
        -- x = goAround i' 100000
        -- ix = fromJust $ 1 `S.elemIndexL` x
        -- nps t = foldl' (\a v -> let nextVal = if (v < (length t - 1)) then t!!(v+1) else length t in take nextVal a ++ [v] ++ drop (nextVal + 1) a) (take (length t) $ repeat 0) [0..length t-1]

        -- i' = map digitToInt i
        -- i' = map digitToInt i ++ [10..100]  -- w/ 1 go, (0.02 secs, 611,848 bytes)
        -- i' = map digitToInt i ++ [10..1000]  -- w/ 1 go, (0.02 secs, 3,997,536 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 1 go, (0.70 secs, 45,434,376 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 10, (1.09 secs, 45,459,480 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 100, (0.84 secs, 45,774,376 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 1000, (1.11 secs, 49,582,712 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 10000, (1.71 secs, 85,856,168 bytes)
        i' = map digitToInt i ++ [10..10000]  -- w/ 100000, (2.22 secs, 449,288,024 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 1000000, (15.84 secs, 4,075,818,336 bytes)
        -- i' = map digitToInt i ++ [10..10000]  -- w/ 10000000, (291.75 secs, 40,340,683,872 bytes)
        -- i' = map digitToInt i ++ [10..100000]  -- w/ 10000, (95.58 secs, 591,615,024 bytes)
        -- i' = map digitToInt i ++ [10..100000]  -- w/ 1000, (124.81 secs, 546,985,440 bytes)
        -- i' = map digitToInt i ++ [10..100000]  -- w/ 10000, (287.04 secs, 591,615,024 bytes)
        -- i' = map digitToInt i ++ [10..100000]  -- w/ 100000, (106.91 secs, 1,014,793,752 bytes)
        -- i' = map digitToInt i ++ [10..100000]  -- w/ 1000000, (171.91 secs, 5,262,364,328 bytes)
        -- i' = map digitToInt i ++ [10..1000000]  -- w/ 1000000, (13793.52 secs, 61,063,655,560 bytes) 389125467: (934001,159792) = 149245887792
        -- i' = map digitToInt i ++ [10..1000000]  -- w/ 1000000, (9556.13 secs, 60,997,735,984 bytes) 156794823: (36807,312400) = 11498506800

        -- i' = map digitToInt i ++ [10..100000]  -- w/ 1 go, (73.42 secs, 533,878,776 bytes)

        -- i' = (map digitToInt i) ++ [10..1000000]
        ps = nps 0 ((head i')-1) (S.fromList (take (length i') $ repeat 0)) i'
        -- z = goAround ps (fromJust $ S.elemIndexL (head i') ps) [] $ (length ps - 1)
        -- z' = getLast $ last z where getLast (_, _, _, _, _, _, _, x) = x
        z = goAround ps (fromJust $ S.elemIndexL (head i' - 1) ps) 100000 $ (length ps - 1)
        -- z = goAround ps (fromJust $ S.elemIndexL (head i' - 1) ps) 10000000 $ (length ps - 1)  -- 
        -- z = goAround ps 6 10 $ (length ps - 1)  -- 
        -- i' = S.fromList ((nps . map digitToInt i) ++ [10..1000000])
        z' = take 3 $ psn 0 (fromJust $ S.elemIndexL 0 z) [] z

    -- print i'
    -- print ps
    -- -- mapM_ print z
    -- -- print $ psn 0 (fromJust $ S.elemIndexL 5 z') [] z'
    -- print z
    -- -- print $ psn 0 (fromJust $ S.elemIndexL 4 z) [] z
    -- print $ take 3 $ psn 0 (fromJust $ S.elemIndexL 0 z) [] z
    print $ (z'!!1) * (z'!!2)

        -- i' = map digitToInt i
    
    -- v <- V.new (length i')
    -- nps 0 ((head i')-1) v i'
    -- let ps = map (\x -> let
    --                 n = if i < length i' then (i'!!i)-1 else (head i') -1
    --                 in V.write v x n
    --             ) i'

    -- print i'
    -- print ps
    -- print $ go i'

-- go :: V.MVector t Int -> [Int]
-- go xs = do
--     v <- V.new $ length xs
--     -- nps 0 ((head xs)-1) v xs
--     mapM (\i -> V.write v i (if i < length xs then (xs!!i)-1 else (head xs) -1)) [0..length xs-1]
--     z1 <- V.read v 0
--     z2 <- V.read v z1
--     -- return $ (z1+1) * (z2+1)
--     return $ U.toList v

psn :: Int -> Int -> [Int] -> S.Seq Int -> [Int]
psn i n a t
    | i == S.length t = a
    | otherwise = let
        n' = S.index t n
        a' = a ++ [n' + 1]
        in psn (i+1) n' a' t

-- psn i n a t
--     | i == V.length t = a
--     | otherwise = do
--         n' <- V.read t n
--         let a' = a ++ [n' + 1]
--         psn (i+1) n' a' t

-- nps n t
--     | n == length (t)
--     | otherwise = let
--     n' = if n < (length t-1) then t!!(n+1) else length t
--     t' = take n t ++ [n'] ++ drop (n + 1) t
--     nps n' t'

-- nps i p t a
--     | i == length t+1 = a
--     | otherwise = let
--         -- v = if i < length t then t!!i else length a
--         v = if i < length t then t!!i else 0
--         -- a' = take p' t ++ [n'] ++ drop (p'+1) t
--         a' = take p a ++ [v] ++ drop (p+1) a
--         in nps (i+1) v t a'
nps :: Int -> Int -> S.Seq Int -> [Int] -> S.Seq Int
nps i p a t
    | i == length t+1 = a
    | otherwise = let
        -- v = if i < length t then t!!i else length a
        v = if i < length t then (t!!i)-1 else (head t) -1  -- If not the last one.
        -- v' = if v == 0 then v = 
        -- a' = take p' t ++ [n'] ++ drop (p'+1) t
        -- a' = take p a ++ [v] ++ drop (p+1) a
        a' = S.take p a S.>< S.singleton v S.>< S.drop (p+1) a
        in nps (i+1) v a' t

-- nps :: Int -> Int -> V.MVector t Int -> [Int] -> V.MVector t Int
-- nps i p a t
--     | i == length t+1 = a
--     | otherwise = runST $ do
--         -- v = if i < length t then t!!i else length a
--         let v = if i < length t then (t!!i)-1 else (head t) -1  -- If not the last one.
--         -- v' = if v == 0 then v = 
--         -- a' = take p' t ++ [n'] ++ drop (p'+1) t
--         -- a' = take p a ++ [v] ++ drop (p+1) a
--         -- a' = V.take p a V.++ V.singleton v V.++ V.drop (p+1) a
--         V.write a p v
--         nps (i+1) v a t


    -- print "Done!"
    -- print i'

    -- print $ fst x
    -- print . concat . map show . drop 1 . rotate $ fst x
    -- print x
    -- print . concat . map show . drop 1 $ rotate x
    -- print $ getn 3 [5, 4, 6, 8, 9, 6]
    -- print $ show(fromJust $ x S.!? (ix + 1)) ++ " and " ++ show(fromJust $ x S.!? (ix + 2))
    
        -- x = goAround i' []  -- 100x (0.83 secs, 23,515,144 bytes) Moving next to front.
        -- x = goAround i' 0 S.empty (length i') -- 100x (0.86 secs, 23,120,280 bytes) Inserts and deletes.
    -- print $ S.index x (S.length x - 1)
        -- x = goAround i' 0 (S.empty S.|> (0, 0, 0, 0, 0, 0, i', i')) (length i') -- 100x (0.86 secs, 23,120,280 bytes) Inserts and deletes.

    --     x = goAround i' 0 (S.empty S.|> (0, 0, 0, 0, 0, 0, i')) (length i') -- 100x (0.86 secs, 23,120,280 bytes) Inserts and deletes.
    -- mapM_ print x

        -- x = goAround i' 0 10000 (length i') -- /w 1m seq (558.82 secs, 333,950,208,184 bytes) Inserts and deletes.
        -- x = goAround i' 0 1000 (length i') -- /w 1m seq (53.88 secs, 33,382,347,184 bytes) Inserts and deletes.
        -- x = goAround i' 0 100 (length i') -- /w 1m seq (4.14 secs, 3,370,678,272 bytes) Inserts and deletes.
    -- print x
    -- print $ S.index x 1


-- goAround :: [Int] -> [[Int]] -> ([Int], [[Int]])
-- goAround b x = let
-- goAround :: [Int] -> Int -> [Int]
-- goAround b x = let
--     curr = fromJust $ b S.!? 0
--     b' = S.drop 1 b
--     n3 = S.take 3 b'  -- instead of drop/take, maybe 3x deleteAt and 3x insertAt? Though, that sounds like 6 temporary seq's
--     b'' = S.drop 3 b'
--     -- subtract 1 from current and keep looking... if you get to lowest, then get highest
--     n = getn (curr-1) (getmax n3 $ S.length b) b''
--     ni = (fromJust $ n `S.elemIndexL` b'') + 1
--     nb = S.take ni b'' S.>< n3 S.>< S.drop ni b'' S.>< S.singleton curr  -- reconstructing the seq is probably slow
--     in if x == 1 then nb else goAround nb (x-1)

-- -- goAround :: S.Seq Int -> Int -> S.Seq (S.Seq Int) -> Int -> S.Seq (S.Seq Int)
-- goAround :: S.Seq Int -> Int -> Int -> Int -> S.Seq Int
-- goAround b i x l = let
--     i' = i `mod` l
--     curr = b `S.index` i'
--     i1 = (i'+1) `mod` l
--     i2 = (i'+2) `mod` l
--     i3 = (i'+3) `mod` l
--     c1 = b `S.index` i1
--     c2 = b `S.index` i2
--     c3 = b `S.index` i3
--     -- adj = 0
--     -- adj1 = if i1 < i then +1 else adj
--     -- adj2 = if i2 < i then adj1+1 else adj1
--     -- adj3 = if i3 < i then adj2+1 else adj2
--     -- We want to reuse first position when deleting to take advantage of auto shrinking. However, need to handle when following elements are not next to first.
--     i2' = if i2 < i1 then i2 else i1
--     i3' = if i3 < i1 then (if i2 < i1 then i2 else i3) else i1
--     b1 = S.deleteAt i3' $ S.deleteAt i2' $ S.deleteAt i1 b
--     n = getn (curr-1) c1 c2 c3 (getn l c1 c2 c3 l)
--     ni = fromJust $ (n `S.elemIndexL` b1)
--     ni1 = (ni+1) `mod` l
--     ni2 = (ni+2) `mod` l
--     ni3 = (ni+3) `mod` l
--     b2 = S.insertAt ni3 c3 $ S.insertAt ni2 c2 $ S.insertAt ni1 c1 b1
--     -- i'' = (if ni2 < ni1 then 1 else 0) + (if ni3 < i' then 1 else 0) + (if ni3 < i' then 1 else 0) + 1 + i'
--     -- To determine next element to process, we want to see how much the sequence shifted to the left because of deletion and then offset with insertion.
--     i1'' = if i1 < i' then 1 else 0
--     i2'' = if i2 < i' then 1 else 0
--     i3'' = if i3 < i' then 1 else 0
--     i'' = if ni < i' then i'+4-(i1'' + i2'' + i3'') else i'+1
--     -- i'' = if ni < i' then i'+4 else i'+1
--     -- subtract 1 from current and keep looking... if you get to lowest, then get highest
--     -- nb = S.take ni b'' S.>< n3 S.>< S.drop ni b'' S.>< S.singleton curr  -- reconstructing the seq is probably slow
--     -- in if S.length x == 100 then (x S.|> b2) else goAround b2 (i+1) (x S.|> b2) l
--     -- in if S.length x == 10 then (x S.|> (i, curr, c1, c2, c3, n, b1, b2)) else goAround b2 (i'') (x S.|> (i, curr, c1, c2, c3, n, b1, b2)) l
--     -- in if S.length x == 500 then (x S.|> (i, curr, c1, c2, c3, n, b2)) else goAround b2 (i'') (x S.|> (i, curr, c1, c2, c3, n, b2)) l
--     in if x == 1 then b2 else goAround b2 (i'') (x-1) l

-- goAround :: S.Seq Int -> Int -> [(Int, Int, Int, Int, Int, Int, Int, S.Seq Int)] -> Int -> [(Int, Int, Int, Int, Int, Int, Int, S.Seq Int)]
goAround :: S.Seq Int -> Int -> Int -> Int -> S.Seq Int
goAround list p acc len = let
    curr = S.index list p
    v1 = S.index list curr
    v2 = S.index list v1
    v3 = S.index list v2
    end = S.index list v3
    n = getn (curr-1) v1 v2 v3  (getn len v1 v2 v3 len)
    pn = S.index list n
    l2 = S.update curr end list  -- Repoint curr to whatever the end of the trio USED to be.
    l3 = S.update n v1 l2  -- Repoint the new value to the beginning of the trio.
    l4 = S.update v3 pn l3  -- Repoint the end of the trio to whatever the new value Used to point to.
    -- in if length acc == (10-1) then (acc ++ [(p, v1, v2, v3, end, n, pn, l4)]) else goAround l4 curr (acc ++ [(p, v1, v2, v3, end, n, pn, l4)]) len
    -- in (acc ++ [(p, v1, v2, v3, end, n, pn, l4)])
    -- in if acc == 1 then l4 else goAround l4 curr (acc-1) len
    in if acc == 1 then l4 else goAround l4 curr (acc-1) len


getn :: Int -> Int -> Int -> Int -> Int -> Int
-- getn a max xs
--     | a `S.elemIndexL` xs /= Nothing = a
--     | otherwise = if a < 1 then max else getn (a-1) max xs
getn n c1 c2 c3 max -- Check that next number is not part of the extracted set.
    -- | n < 1 = max
    | n < 0 = max
    | (c1 == n || c2 == n || c3 == n) = getn (n-1) c1 c2 c3 max
    | otherwise = n

-- rotate :: [Int] -> [Int]
-- rotate a@(x:xs) = if x == 1 then a else rotate (xs++[x])

-- -- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
-- rotate :: Int -> [a] -> [a]
-- rotate _ [] = []
-- rotate n xs = zipWith const (drop n (cycle xs)) xs