import Data.List (foldl', elemIndex)
import Data.List.Split (splitOn)
-- import qualified Data.Map as M
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromJust)
-- import qualified Data.Vector.Unboxed.Mutable as V
-- import Data.Vector.Unboxed (toList, freeze)

main = do
    -- let input = "0,3,6"
    -- let input = "1,3,2"
    -- let input = "2,1,3"
    -- let input = "3,1,2"
    let input = "0,13,16,17,1,10,6"
        i = map (\a -> read a::Int) $ splitOn "," input
        i' = zip (init i) [1..]
        i'' = foldl' (\a x -> M.insert (fst x) (snd x) a) M.empty i'
        -- z = nextTurn (last i) i'' (M.size i'' + 1) (init i)

    print i'
    -- print $ last z
    -- print $ nextTurn 17 i'' (M.size i'')
    print $ nextTurn (last i) i'' (M.size i'' + 1)
    -- print $ nextTurn (last i) i'' (M.size i'' + 1) (init i)
    -- print $ last $ nextTurn (last i) i'' (M.size i'' + 1) (init i)

nextTurn lastNum allTurns lastTurnNum = let
-- nextTurn lastNum allTurns lastTurnNum trackNums = let
    i = M.lookup lastNum allTurns
    thisNum = if i == Nothing then 0 else lastTurnNum - (fromJust i)
    thisTurnNum = lastTurnNum + 1
    in if thisTurnNum == 30000001 then lastNum
    -- in if thisTurnNum == 30001 then (trackNums ++ [lastNum])
    -- in if thisTurnNum == 30000001 then (trackNums ++ [lastNum])
        else nextTurn thisNum (M.insert lastNum lastTurnNum allTurns) thisTurnNum
        -- else nextTurn thisNum (M.insert lastNum lastTurnNum allTurns) thisTurnNum (trackNums ++ [lastNum])
    -- in nextNum

-- main = do
--     let input = "0,13,16,17,1,10,6"
--         i = map (\a -> read a::Int) $ splitOn "," input
--         lastSeen = M.fromList (zip [0..] (take 30000000 $ repeat 0))
--         lastSeen' = foldl' (\a x -> M.alter (\b -> Just ((fromJust $ x `elemIndex` i) + 1)) x a) lastSeen (init i)
--         z = nextTurn (last i) lastSeen' (length i)
    
--     print i
--     -- print $ take 10 $ M.toList lastSeen'
--     print z

-- nextTurn :: Int -> M.IntMap Int -> Int -> Int
-- nextTurn lastNum allTurns lastTurnNum = let
-- -- nextTurn lastNum allTurns lastTurnNum trackNums = let
--     i = fromJust $ M.lookup lastNum allTurns
--     thisNum = if i == 0 then 0 else lastTurnNum - i
--     thisTurnNum = lastTurnNum + 1
--     in if thisTurnNum == 30000001 then lastNum
--     -- in if thisTurnNum == 30001 then (trackNums ++ [lastNum])
--     -- in if thisTurnNum == 30000001 then (trackNums ++ [lastNum])
--         else nextTurn thisNum (M.alter (\b -> Just lastTurnNum) lastNum allTurns) thisTurnNum
--         -- else nextTurn thisNum (M.insert lastNum lastTurnNum allTurns) thisTurnNum (trackNums ++ [lastNum])
--     -- in nextNum

-- main = do
--     -- let input = "0,3,6"
--     -- let input = "1,3,2"
--     -- let input = "2,1,3"
--     -- let input = "3,1,2"
--     lastSeen <- V.replicate 30 (0::Int)
--     V.write lastSeen 0 1
--     V.write lastSeen 13 2
--     V.write lastSeen 16 3
--     V.write lastSeen 17 4
--     V.write lastSeen 1 5
--     V.write lastSeen 10 6

--     let input = "0,13,16,17,1,10,6"
--         i = map (\a -> read a::Int) $ splitOn "," input
--         -- lastSeen' = foldl' (\a x -> V.write a x ((fromJust $ x `elemIndex` i) + 1)) lastSeen (init i)
    

--         -- lastSeen' = initV lastSeen i 1
--         z = nextTurn (last i) lastSeen (length i)

--     im_v <- freeze lastSeen
--     -- print $ take 10 im_v
--     -- print $ take 10 $ toList lastSeen
--     print i
--     print im_v
--     -- print z

-- -- initV v [] y = return v
-- -- initV v (x:xs) y = do V.write (initV v xs (y + 1)) x y

-- nextTurn :: V.Unbox a => Int -> V.MVector a -> Int -> Int
-- nextTurn lastNum allTurns lastTurnNum = do
-- -- nextTurn lastNum allTurns lastTurnNum trackNums = let
--     i <- V.read allTurns lastNum
--     let thisNum = if i == 0 then 0 else lastTurnNum - i
--         thisTurnNum = lastTurnNum + 1
--         in if thisTurnNum == 30 then lastNum
--         -- in if thisTurnNum == 30001 then (trackNums ++ [lastNum])
--         -- in if thisTurnNum == 30000001 then (trackNums ++ [lastNum])
--             else do
--                 V.write allTurns lastNum lastTurnNum
--                 nextTurn thisNum allTurns thisTurnNum
--             -- else nextTurn thisNum (M.insert lastNum lastTurnNum allTurns) thisTurnNum (trackNums ++ [lastNum])
--         -- in nextNum