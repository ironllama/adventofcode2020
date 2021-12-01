import Data.List.Split
import Data.List (foldl', elemIndex)
import qualified Data.Map as M
import Data.Maybe

main = do
--     let i = "mask = 000000000000000000000000000000X1001X\n\
-- \mem[42] = 100\n\
-- \mask = 00000000000000000000000000000000X0XX\n\
-- \mem[26] = 1"
    i <- readFile "day14.in"

    let i' = lines i
    -- print i'
    -- print $ binStringToInt "0000000000000000000000000000101"
    -- print $ intToBinString 102

    -- print $ foldr (\t a -> (show t) ++ a) "" $ intToBinString' 102
    -- print $ addMem "46" "10111" "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" M.empty
    -- print y

        y = f i' "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" M.empty

    -- print y
    print $ foldr (\x a -> snd x + a) 0 $ M.toList y

f :: [String] -> String -> M.Map Int Int -> M.Map Int Int
f [] _ l = l
f (x:xs) m l = let
    x' = splitOn " = " x
    m' = if head x' == "mask" then last x' else m
    in if take 3 (head x') == "mem"
        then f xs m' $ addMem (drop 4 $ init $ head x') (last x') m' l
        else f xs m' l

addMem :: String -> String -> String -> M.Map Int Int -> M.Map Int Int
addMem x y m l = let
    x' = intToBinString (read x::Int)
    y' = read y::Int
    x'' = (concat $ take (length m - length x') $ repeat "0") ++ x'
    bs = foldr (\d acc -> case fst d of
                            '0' -> (snd d):acc
                            '1' -> '1':acc
                            'X' -> 'X':acc
                            ) [] (zip m x'')
    xs = getAddrs bs []
    -- in map (\a -> M.insert a y' l) $ getAddrs bs []
    in addMap xs y' l

addMap :: [Int] -> Int -> M.Map Int Int -> M.Map Int Int
addMap [] _ l = l
addMap (x:xs) y l = addMap xs y (M.insert x y l)

getAddrs :: String -> [Int] -> [Int]
getAddrs bs m = let
    ix = fromMaybe (-1) $ 'X' `elemIndex` bs
    in if ix == -1 then binStringToInt bs:m
        else concat $ map (\a -> getAddrs (take ix bs ++ a ++ (drop (ix + 1) bs)) m) ["0", "1"]

binStringToInt :: String -> Int
binStringToInt = foldl' (\accum x -> (accum * 2) + (if x == '0' then 0 else 1)) 0  -- Using foldl' instead of foldl for performance

-- intToBinString :: Int -> String
-- intToBinString x = showIntAtBase 2 intToDigit x ""

intToBinString :: Int -> String
intToBinString x = reverse $ concat $ intToBinString' x
  where
    intToBinString' 0 = []
    intToBinString' y = let (a,b) = quotRem y 2 in [show b] ++ intToBinString' a