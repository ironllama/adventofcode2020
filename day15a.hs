import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromJust)

main = do
    -- let input = "0,3,6"
    -- let input = "1,3,2"
    -- let input = "2,1,3"
    -- let input = "3,1,2"
    let input = "0,13,16,17,1,10,6"
        i = map (\a -> read a::Int) $ splitOn "," input
        i' = zip (init i) [1..]
        i'' = foldl' (\a x -> M.insert (fst x) (snd x) a) M.empty i'

    print i'
    -- print $ nextTurn 17 i'' (M.size i'')
    print $ last $ nextTurn (last i) i'' (M.size i'' + 1) (init i)

nextTurn lastNum allTurns lastTurnNum trackNums = let
    i = M.lookup lastNum allTurns
    thisNum = if i == Nothing then 0 else lastTurnNum - (fromJust i)
    thisTurnNum = lastTurnNum + 1
    in if thisTurnNum == 2021 then (trackNums ++ [lastNum])
        else nextTurn thisNum (M.insert lastNum lastTurnNum allTurns) thisTurnNum (trackNums ++ [lastNum])
    -- in nextNum