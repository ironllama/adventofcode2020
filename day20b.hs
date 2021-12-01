import Data.List (nub, transpose, elemIndex)
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromJust)

main = do
--     let i ="Tile 2311:\n\
-- \..##.#..#.\n\
-- \##..#.....\n\
-- \#...##..#.\n\
-- \####.#...#\n\
-- \##.##.###.\n\
-- \##...#.###\n\
-- \.#.#.#..##\n\
-- \..#....#..\n\
-- \###...#.#.\n\
-- \..###..###\n\
-- \\n\
-- \Tile 1951:\n\
-- \#.##...##.\n\
-- \#.####...#\n\
-- \.....#..##\n\
-- \#...######\n\
-- \.##.#....#\n\
-- \.###.#####\n\
-- \###.##.##.\n\
-- \.###....#.\n\
-- \..#.#..#.#\n\
-- \#...##.#..\n\
-- \\n\
-- \Tile 1171:\n\
-- \####...##.\n\
-- \#..##.#..#\n\
-- \##.#..#.#.\n\
-- \.###.####.\n\
-- \..###.####\n\
-- \.##....##.\n\
-- \.#...####.\n\
-- \#.##.####.\n\
-- \####..#...\n\
-- \.....##...\n\
-- \\n\
-- \Tile 1427:\n\
-- \###.##.#..\n\
-- \.#..#.##..\n\
-- \.#.##.#..#\n\
-- \#.#.#.##.#\n\
-- \....#...##\n\
-- \...##..##.\n\
-- \...#.#####\n\
-- \.#.####.#.\n\
-- \..#..###.#\n\
-- \..##.#..#.\n\
-- \\n\
-- \Tile 1489:\n\
-- \##.#.#....\n\
-- \..##...#..\n\
-- \.##..##...\n\
-- \..#...#...\n\
-- \#####...#.\n\
-- \#..#.#.#.#\n\
-- \...#.#.#..\n\
-- \##.#...##.\n\
-- \..##.##.##\n\
-- \###.##.#..\n\
-- \\n\
-- \Tile 2473:\n\
-- \#....####.\n\
-- \#..#.##...\n\
-- \#.##..#...\n\
-- \######.#.#\n\
-- \.#...#.#.#\n\
-- \.#########\n\
-- \.###.#..#.\n\
-- \########.#\n\
-- \##...##.#.\n\
-- \..###.#.#.\n\
-- \\n\
-- \Tile 2971:\n\
-- \..#.#....#\n\
-- \#...###...\n\
-- \#.#.###...\n\
-- \##.##..#..\n\
-- \.#####..##\n\
-- \.#..####.#\n\
-- \#..#.#..#.\n\
-- \..####.###\n\
-- \..#.#.###.\n\
-- \...#.#.#.#\n\
-- \\n\
-- \Tile 2729:\n\
-- \...#.#.#.#\n\
-- \####.#....\n\
-- \..#.#.....\n\
-- \....#..#.#\n\
-- \.##..##.#.\n\
-- \.#.####...\n\
-- \####.#.#..\n\
-- \##.####...\n\
-- \##..#.##..\n\
-- \#.##...##.\n\
-- \\n\
-- \Tile 3079:\n\
-- \#.#.#####.\n\
-- \.#..######\n\
-- \..#.......\n\
-- \######....\n\
-- \####.#..#.\n\
-- \.#...#.##.\n\
-- \#.#####.##\n\
-- \..#.###...\n\
-- \..#.......\n\
-- \..#.###..."
    i <- readFile "day20.in"

    let i' = foldr f [(0, [])] $ lines i
                where f x a@(y:ys)
                        | x == "" = (0, []):a
                        | head x == 'T' = (read (init $ last $ words x)::Int, snd y):ys
                        | otherwise = (fst y, x:snd y):ys
        im = M.fromList i'
        -- ns = foldr (\tile a -> let
        --             tile' = snd tile
        --             t = head tile'
        --             b = last tile'
        --             l = map (\x -> head x) tile'
        --             r = map (\x -> last x) tile'
        --             in M.insert (fst tile) (tileToSideInts $ snd tile) a
        --         ) M.empty i'
        ns = M.fromList $ map (\x -> (fst x, tileToSideInts $ snd x)) i'

        -- matches = map (\tile -> (fst tile, foldr f 0 $ snd tile)
        --                 where f n a = a + (length $ getMatchTile (fst tile) n ns)  -- Doesn't like where inside the map?
        --             ) ns
        -- matches = M.mapWithKey (\k v -> let f n a = (length $ getMatchTile k n ns) + a in foldr f 0 v) ns
        -- matches = M.mapWithKey (\k v -> let f n a = let m = getMatchTile k n ns in if m /= -1 then m:a else a in foldr f [] v) ns
        matches = M.mapWithKey (\k v -> foldr (\n a ->
                                            let m = getMatchTile k n ns in if m /= -1 then m:a else a
                                        ) [] v
                                ) ns

        -- corners = M.toList $ M.filter (==4) matches
        corners = M.toList $ M.filter ((==4) . length) matches
        fstCorner = fst $ head corners
        -- fstCorner = fst $ last corners
        -- fstCorner = fst $ corners!!2
        -- fstCorner' = setFstCorner (fstCorner, im M.! fstCorner) ns matches
        -- nextTile = findNextTile fstCorner' ns im
        -- nextTile' = findNextTile nextTile ns im
        rowSize = round . sqrt . fromIntegral $ length i'
        allRows = getRows rowSize (fstCorner, im M.! fstCorner) ns matches im []
        noBorders = removeBorders allRows
        pic = pretty rowSize "" noBorders []
        monster = [(0,0), (1,-1), (4,-1), (5,0), (6,0), (7,-1), (10,-1), (11,0), (12,0), (13,-1), (16,-1), (17,0), (18,0), (18,1), (19,0)]

    -- print i'
    -- print ns
    -- print $ getMatchTile 2311 210 ns
    -- print matches
    -- print corners
    -- print $ map (\row -> ) [0..(length $ snd $ head i') - 1]
    -- print $ product $ map (\a -> fst a) corners
    -- putStrLn $ pretty (round . sqrt . fromIntegral $ length i') "" i' []

    -- let fstCorner = fst $ head corners
    -- print $ (fstCorner, ns M.! fstCorner)  -- Corner piece with id
    -- -- print $ map (\x -> (x, ns M.! x)) $ nub $ matches M.! fstCorner  -- Matches for corner piece, dec sides
    -- -- print $ concat $ map (\x -> ns M.! x) $ nub $ matches M.! fstCorner  -- Matches for corner piece, dec sides
    -- print $ map (\x ->
    --         let matchsides = concat $ map (\x -> ns M.! x) $ nub $ matches M.! fstCorner
    --         in x `elem` matchsides
    --     ) (take 4 $ ns M.! fstCorner)
    -- -- print $ im M.! fstCorner  -- Corner piece, str sides
    -- print $ setFstCorner (fstCorner, im M.! fstCorner) ns matches
    -- print fstCorner'
    -- let rightNum = (tileToSideInts $ snd fstCorner')!!1
    --     matchSides = ns M.! (getMatchTile (fst fstCorner') rightNum ns)
    -- -- print rightNum
    -- -- print matchSides
    -- case fromJust $ rightNum `elemIndex` matchSides of
    --     3 -> print "Dude."
    --     _ -> print "Nope."
    -- print allRows
    -- putStrLn $ pretty 3 " " [fstCorner', nextTile, nextTile'] []
    -- putStrLn $ pretty rowSize " " allRows []
    -- putStrLn pic
    -- print $ lines pic

    let picLines = lines pic
        numHashes = sum $ map (length . filter (=='#')) picLines
        numMonsters = scanGrid 1 picLines monster
        finalHashes = numHashes - (numMonsters * (length monster))

    print numHashes

    -- let picLines = flipOver $ lines pic
        -- yLen = length picLines - 1
        -- xLen = length (picLines!!0) - 1
        -- newPic = map (\y ->
        --                 map (\x ->
        --                         if (checkMonster x y picLines monster) then 'O' else (picLines!!y)!!x
        --                     ) [0..(xLen - 19)]
        --             ) [1..(yLen - 1)]

    print numMonsters
    print finalHashes
    -- mapM_ putStrLn newPic


    -- let pic' = map (\y ->
    --                 map (\x ->
    --                         if y == 0 || y == (length pic - 1) then x
    --                         else if (pic!!y)!!x == '#' then let mon = testForMonster x y pic in 
    --                             else x
    --                     ) snd y
    --             ) pic

scanGrid :: Int -> [String] -> [(Int, Int)] -> Int
scanGrid iter gridLines monster = let
    yLen = length gridLines - 1
    xLen = length (gridLines!!0) - 1
    numMonsters = sum $ map (\y ->
                            sum $ map (\x ->
                                    if (checkMonster x y gridLines monster) then 1 else 0
                                ) [0..(xLen - 19)]
                        ) [1..(yLen - 1)]
    in if numMonsters > 0 then numMonsters
        else if iter /= 4 && iter < 8 then scanGrid (iter+1) (rotateRight gridLines) monster
            else if iter == 4 then scanGrid (iter+1) (flipOver gridLines) monster
                else numMonsters

checkMonster :: Int -> Int -> [String] -> [(Int, Int)] -> Bool
checkMonster x y grid monster = let
    total = sum $ map (\v -> let
        newY = (snd v) + y
        newX = (fst v) + x
        gridP = (grid!!newY)!!newX
        in if gridP == '#' then 1 else 0
        ) monster
    in if total == length monster then True else False

getRows :: Int -> (Int, [String]) -> M.IntMap [Int] -> M.IntMap [Int] -> M.IntMap [String] -> [(Int, [String])] -> [(Int, [String])]
getRows rowSize tile ns matches im acc
    -- | length acc == 37 = acc
    | length acc == M.size im = acc
    | length acc == 0 = let acc' = acc ++ [setFstCorner tile ns matches] in getRows rowSize (last acc') ns matches im acc'
    | otherwise = let
        srcTile = if (length acc) `mod` rowSize == 0 then acc!!(length acc - rowSize) else tile
        srcSide = if (length acc) `mod` rowSize == 0 then 2 else 1
        tile' = findNextTile srcSide srcTile ns im
        in getRows rowSize tile' ns matches im (acc ++ [tile'])

findNextTile :: Int -> (Int, [String]) -> M.IntMap [Int] -> M.IntMap [String] -> (Int, [String])
findNextTile side tile ns im = let
    sideNum = (tileToSideInts $ snd tile)!!side
    match = getMatchTile (fst tile) sideNum ns  -- Get tile match for right side of source
    matchSides = ns M.! match
    matchStr = im M.! match
    matchSideIdx = fromJust $ sideNum `elemIndex` matchSides
    alts1 = [
            (match, flipOver . rotateRight $ matchStr),  -- *
            (match, flipOver $ matchStr),  -- *
            (match, rotateRight $ matchStr),  -- *
            (match, matchStr),  -- *
            (match, rotateRight . rotateRight . rotateRight $ matchStr),  -- *
            (match, rotateRight . rotateRight $ matchStr),  -- *
            (match, flipOver . rotateRight . rotateRight . rotateRight $ matchStr),  -- *
            (match, flipOver . rotateRight . rotateRight $ matchStr)  -- NEVER?
        ]
    alts2 = [
            (match, matchStr),  -- *
            (match, rotateRight . rotateRight . rotateRight $ matchStr),  -- NEVER?  -- *
            (match, flipOver . rotateRight . rotateRight $ matchStr),  -- NEVER? -- *
            (match, flipOver . rotateRight $ matchStr), -- *
            (match, flipOver $ matchStr),  -- *
            (match, flipOver . rotateRight . rotateRight . rotateRight $ matchStr),  -- *
            (match, rotateRight . rotateRight $ matchStr),  -- NEVER?
            (match, rotateRight $ matchStr)  -- *
        ]
    in case side of
        1 -> alts1!!matchSideIdx
        2 -> alts2!!matchSideIdx

setFstCorner :: (Int, [String]) -> M.IntMap [Int] -> M.IntMap [Int] -> (Int, [String])
setFstCorner tile ns matches = let
    config = map (\x ->
            let matchsides = concat . map (\x -> ns M.! x) . nub $ (M.!) matches $ fst tile
            in x `elem` matchsides
        ) (take 4 . tileToSideInts $ snd tile)
    in if config == [False, True, True, False] then tile
        else setFstCorner (fst tile, rotateRight $ snd tile) ns matches

removeBorders :: [(Int, [String])] -> [(Int, [String])]
removeBorders xs = map (\a -> (fst a, map (tail . init) (tail $ init $ snd a))) xs

tileToSideInts :: [String] -> [Int]
tileToSideInts tile' = let
    t = head tile'
    b = last tile'
    l = map (\x -> head x) tile'
    r = map (\x -> last x) tile'
    in toBinNum $ [t, r, b, l, reverse t, reverse r, reverse b, reverse l]

toBinNum :: [String] -> [Int]
toBinNum = map (\x -> binStringToInt $ map (\y -> if y == '#' then '1' else '0') x)

binStringToInt :: String -> Int
binStringToInt = foldr (\x accum -> (accum * 2) + (if x == '0' then 0 else 1)) 0 . reverse -- Using foldl' instead of foldl for performance

-- getMatchTile :: Int -> Int -> M.IntMap [Int] -> M.IntMap [Int]
-- getMatchTile i n = M.foldrWithKey (\k v a -> if i /= k && n `elem` v then M.insert k v a else a) M.empty
getMatchTile :: Int -> Int -> M.IntMap [Int] -> Int  -- Given one side dec num, gets tile id that has a matching side
getMatchTile i n = M.foldrWithKey (\k v a -> if a == -1 && i /= k && n `elem` v then k else a) (-1)

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

flipOver :: [[a]] -> [[a]]
flipOver = map (reverse)

pretty :: Int -> String -> [(Int, [String])] -> String -> String
pretty _ _ [] acc = acc
pretty size sep xs acc = let
    row = take size xs
    acc' = (foldr (\y a ->
                (foldr (\x a -> a ++ sep ++ ((snd x)!!y)) a $ reverse row) ++ "\n"
            ) acc $ reverse [0..(length $ snd $ head row)-1]) ++ (if length sep > 0 then "\n" else "")
    in pretty size sep (drop size xs) acc'