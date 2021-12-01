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
        ls' =  map (\x -> ((unwords $ take 3 x), (unwords $ drop 4 x))) ls

        x = ["shiny gold bags"]
        -- x = ["faded blue bags"]
        -- x = ["muted yellow bags","dark olive bags","vibrant plum bags"]
        -- x = ["muted yellow bags"]
        -- x = ["light red bags","dark orange bags"]
        -- x = ["shiny gold bags", "faded blue bags"]
        -- x = ["light red bags"]
        y = f x ls' []
        z = length y - 1

    -- print ls
    -- print ls'
    -- print y
    print z

f :: [String] -> [(String, String)] -> [String] -> [String]
-- f :: [a] -> [a] -> [a]
f [] _ acc = acc
-- f (x:xs) ys = f' x ys:f xs ys
-- f xs ys = concat $ map (\t -> f' t ys) xs
-- f xs ys = f (concat $ map (\t -> f' t ys) xs) ys
-- f (x:xs) ys acc = f' x ys
-- f (x:xs) ys = f ((f' x ys) ++ xs) ys
-- f (x:xs) ys acc = let
--     n = (f' x ys)
--     in nub $ (f (n ++ xs) ys) ++ n
f (x:xs) ys acc
    | x `elem` acc = f xs ys acc  -- if current bag to visit has already been visited, go to the next one
    | otherwise = let
    x' = f' x ys  -- visit and get list of bags inside the bag
    in f (x' ++ xs) ys (x:acc)  -- add the bags inside to the list to visit and add the visted bag to the accumulator

-- Return list of matches using first list against all the second part of the second pairs
-- f' xs ys = map (\x -> foldr (\y acc -> if x `substring` (snd y) then (fst y):acc else acc) [] ys) xs
f' [] _ = []
f' x ys = foldr (\y acc -> if (init x) `substring` (snd y) then (fst y):acc else acc) [] ys

-- From https://stackoverflow.com/questions/30588221/check-a-string-if-it-contains-a-given-substring-and-return-boolean
substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys | prefix xs ys = True | substring xs (tail ys) = True | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys