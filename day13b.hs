-- import Data.List
import Data.List.Split

main = do
--     let i = "939\n\
-- \7,13,x,x,59,x,31,19"
    -- i <- readFile "day13.in"

    -- let i' = map (\a -> (read (fst a) :: Int, snd a)) $ filter (\a -> fst a /= "x") $ zip (splitOn "," $ last $ lines i) [0..]
        -- i'' = map (\a -> [(fst a), (((fst a) * 2) + (snd a))..]) i'
        -- i''' = foldl1 intersect i''
        -- ys = foldr (\a acc -> if a /= "x" then (read a :: Int):acc else acc ) [] $ splitOn "," $ last i'
        -- z = findTime x ys

    -- print i'
    -- print ys
    -- print $ take 5 ys'
    -- print $ take 1 z
    -- print z
    -- print $ head [ x | x <- [7, (7 * 2)..],
    --                 (x + 1) `mod` 13 == 0
    --                 && (x + 4) `mod` 59 == 0
    --                 && (x + 6) `mod` 31 == 0
    --                 && (x + 7) `mod` 19 == 0
    --                 ]
    -- print $ head [ x | x <- [1789, (1789 * 2)..],
    --                 (x + 1) `mod` 37 == 0
    --                 && (x + 2) `mod` 47 == 0
    --                 && (x + 3) `mod` 1889 == 0
    --                 ]
    -- print $ head [ x | x <- [19, (19 * 2)..],
    --                 (x + 9) `mod` 41 == 0
    --                 && (x + 13) `mod` 37 == 0
    --                 && (x + 19) `mod` 821 == 0
    --                 && (x + 32) `mod` 13 == 0
    --                 && (x + 36) `mod` 17 == 0
    --                 && (x + 48) `mod` 29 == 0
    --                 && (x + 50) `mod` 463 == 0
    --                 && (x + 73) `mod` 23 == 0
    --             ]
    -- print $ (head [ x | x <- [821, (821 * 2)..],
    --                 (x - 10) `mod` 41 == 0
    --                 && (x - 6) `mod` 37 == 0
    --                 && (x - 19) `mod` 19 == 0
    --                 && (x + 13) `mod` 13 == 0
    -- --                 && (x + 36) `mod` 17 == 0
    -- --                 && (x + 48) `mod` 29 == 0
    -- --                 && (x + 50) `mod` 463 == 0
    -- --                 && (x + 73) `mod` 23 == 0
    --             ]) - 19

    -- let a = [ x | x <- [821, (821 * 2)..], (x + 31) `mod` 463 == 0]
    -- print $ head a
    -- let a' = [head a, (head a) + (821 * 463)..]
    -- let b = [ x | x <- a', (x - 10) `mod` 41 == 0]
    -- print $ head b
    -- let b' = [head b, (head b) + (821 * 463 * 41)..]
    -- let c = [ x | x <- b', (x - 6) `mod` 37 == 0]
    -- print $ head c
    -- let c' = [head c, (head c) + (821 * 463 * 41 * 37)..]
    -- let d = [ x | x <- c', (x - 19) `mod` 19 == 0]
    -- print $ head d
    -- let d' = [head d, (head d) + (821 * 463 * 41 * 37 * 19)..]
    -- let e = [ x | x <- d', (x + 13) `mod` 13 == 0]
    --     e' = [head e, (head e) + (821 * 463 * 41 * 37 * 19 * 13)..]
    --     f = [ x | x <- e', (x + 17) `mod` 17 == 0]
    --     f' = [head f, (head f) + (821 * 463 * 41 * 37 * 19 * 13 * 17)..]
    --     g = [ x | x <- f', (x + 29) `mod` 29 == 0]
    --     g' = [head g, (head g) + (821 * 463 * 41 * 37 * 19 * 13 * 17 * 29)..]
    --     h = [ x | x <- g', (x + 54) `mod` 23 == 0]
    -- print $ (head h) - 19
    -- 192114
    -- 13876542
    -- 528182961
    -- 6294648871
    -- 93944930684

--     let i = "939\n\
-- \7,13,x,x,59,x,31,19"
    i <- readFile "day13.in"

    -- Create a list of pairs, containing the value and position
    let i' = map (\a -> (read (fst a) :: Int, snd a)) $ filter (\a -> fst a /= "x") $ zip (splitOn "," $ last $ lines i) [0..]
    print $ nextBus i' [] 1

nextBus :: [(Int, Int)] -> [Int] -> Int -> Int
nextBus [] ys _ = head ys  -- Final value!
nextBus (x:xs) [] acc = nextBus xs [(fst x), ((fst x) * 2)..] (fst x)  -- Initializer
nextBus (x:xs) ys acc = let
    a = [ n | n <- ys, (n + (snd x)) `mod` (fst x) == 0]  -- Recursively apply the mod filter.
    acc' = acc * (fst x)
    -- b = a -- Reusing the lazy 'mod' list will kill the performance. (SSSSLLLLLOOOOOWWWW)
    b = [head a, ((head a) + acc')..]  -- Recreate ys using multiplication. Based on pattern found while sketching out on paper.
    in nextBus xs b acc'
