-- import Numeric (showIntAtBase)
-- import Data.Char (intToDigit)
import Data.List.Split
import Data.List (foldl')
import qualified Data.Map as M

main = do
--     let i = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
-- \mem[8] = 11\n\
-- \mem[7] = 101\n\
-- \mem[8] = 0"
--     let i = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
-- \mem[8] = 11\n\
-- \mem[7] = 101"
--     let i = "mem[8] = 11\n\
-- \mem[7] = 101\n\
-- \mem[8] = 0"
    i <- readFile "day14.in"

    let i' = lines i
        -- y = f i' "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" []

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
    x' = read x::Int
    y' = intToBinString (read y::Int)
    y'' = (concat $ take (length m - length y') $ repeat "0") ++ y'
    n = foldr (\d acc -> if fst d /= 'X' then (fst d):acc else (snd d):acc) [] (zip m y'')
    in M.insert x' (binStringToInt n) l

binStringToInt :: String -> Int
binStringToInt = foldl' (\accum x -> (accum * 2) + (if x == '0' then 0 else 1)) 0  -- Using foldl' instead of foldl for performance

-- intToBinString :: Int -> String
-- intToBinString x = showIntAtBase 2 intToDigit x ""

intToBinString :: Int -> String
intToBinString x = reverse $ concat $ intToBinString' x
  where
    intToBinString' 0 = []
    intToBinString' y = let (a,b) = quotRem y 2 in [show b] ++ intToBinString' a