import qualified Data.Map as M
import Data.List.Split
import Data.Char

main = do
--     let i ="light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
-- \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
-- \bright white bags contain 1 shiny gold bag.\n\
-- \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
-- \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
-- \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
-- \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
-- \faded blue bags contain no other bags.\n\
-- \dotted black bags contain no other bags."
    i <- readFile "day07.in"

    let ls = map (words) $ lines i
        ls' =  M.fromList $ map (\x -> ((unwords $ take 2 x), (unwords $ drop 4 x))) ls

        x = ["1 shiny gold"]
        -- x = ["1 dark olive","3 vibrant plum"]
        -- x = ["5 faded blue", "6 dotted black"]
        -- x = ["light red bags"]
        -- y = f x ls' []
        -- z = length y - 1
        -- x = "1 shiny gold"
        -- y = f' x ls'
        y = sum $ f x ls' []

    -- print ls
    -- print ls'
    -- print $ ll x ls'
    -- print $ f' x ls'
    print y
    -- print z

f :: [String] -> M.Map String String -> [Int] -> [Int]
-- -- f :: [a] -> [(a, a)] -> [a] -> [a]
f [] _ acc = acc
-- -- f (x:xs) ys acc = f' x ys
f (x:xs) ys acc
    -- | x `elem` acc = f xs ys acc  -- if current bag to visit has already been visited, go to the next one
    | otherwise = let
    x' = f' x ys  -- visit and get list of bags inside the bag
    x'' = map (\a -> unwords $ take 3 a) x'  -- just get the bag num and colors
    acc' = map (\a -> let b = unwords $ take 1 a in if (b /= "" && isDigit (b!!0)) then read b :: Int else 0) x'  -- get just the numbers
    acc'' = if sum acc' == 0 then acc else (sum acc'):acc
    in if (unwords x'' == "") then f xs ys acc'' else f (x'' ++ xs) ys acc''  -- add the bags inside to the list to visit and add the visted bag to the accumulator
    -- in if (unwords x'' == "other bags." || unwords x'' == "") then f xs ys acc'':acc else f (x'' ++ xs) ys acc'':acc  -- add the bags inside to the list to visit and add the visted bag to the accumulator
    -- in x''
    -- in [sum acc']

-- Return list of matches using first list against all the second part of the second pairs
-- f' :: String -> [(String, String)] -> [String]
-- f' :: a -> [(a, a)] -> [a]
-- f' [] _ = []
-- f' x ys = foldr (\y acc -> if (init x) `substring` (snd y) then (fst y):acc else acc) [] ys
f' x ys = let
    n = read (unwords $ take 1 $ words x) :: Int
    x' = unwords $ drop 1 $ words x
    x'' = words $ ll x' ys
    in foldr (\a accum@(b:bs) -> if last a == ',' then [init a]:accum else if isDigit (a!!0) then ((show ((read a :: Int) * n)):b):bs else (a:b):bs) [[]] x''
    -- in n

ll x ys = case M.lookup x ys of
    Nothing -> []
    Just y -> y

-- From https://stackoverflow.com/questions/30588221/check-a-string-if-it-contains-a-given-substring-and-return-boolean
substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys | prefix xs ys = True | substring xs (tail ys) = True | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys