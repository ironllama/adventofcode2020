import Data.List (nub, transpose, elemIndex)
import qualified Data.IntMap.Strict as M
import Data.Maybe (fromJust)

main = do
    i <- readFile "day20.in"

    let i' = foldr f [(0, [])] $ lines i
                where f x a@(y:ys)
                        | x == "" = (0, []):a
                        | head x == 'T' = (read (init $ last $ words x)::Int, snd y):ys
                        | otherwise = (fst y, x:snd y):ys
        im = M.fromList i'
        ns = M.fromList $ map (\x -> (fst x, tileToSideInts $ snd x)) i'
        matches = M.mapWithKey (\k v -> foldr (\n a ->
                                            let m = getMatchTile k n ns in if m /= -1 then m:a else a
                                        ) [] v
                                ) ns

        fstCorner = fst . head . M.toList $ M.filter ((==4) . length) matches
        rowSize = round . sqrt . fromIntegral $ length i'
        allRows = getRows rowSize (fstCorner, im M.! fstCorner) ns matches im []
        picLines = lines $ pretty rowSize "" (removeBorders allRows) []
        monster = [(0,0), (1,-1), (4,-1), (5,0), (6,0), (7,-1), (10,-1), (11,0), (12,0), (13,-1), (16,-1), (17,0), (18,0), (18,1), (19,0)]
        numHashes = sum $ map (length . filter (=='#')) picLines
        numMonsters = scanGrid 1 picLines monster
        finalHashes = numHashes - (numMonsters * (length monster))

    print finalHashes

scanGrid iter lines mon = let
    yLen = length lines - 1
    xLen = length (lines!!0) - 1
    nMon = sum $ map (\y ->
                            sum $ map (\x ->
                                    if (checkMonster x y lines mon) then 1 else 0
                                ) [0..(xLen - 19)]
                        ) [1..(yLen - 1)]
    in if nMon > 0 then nMon
        else if iter /= 4 && iter < 8 then scanGrid (iter+1) (rotR lines) mon
            else if iter == 4 then scanGrid (iter+1) (flpO lines) mon
                else nMon

checkMonster x y grid monster = let
    total = sum $ map (\v -> let
        newY = (snd v) + y
        newX = (fst v) + x
        gridP = (grid!!newY)!!newX
        in if gridP == '#' then 1 else 0
        ) monster
    in if total == length monster then True else False

getRows rowSize tile ns matches im acc
    -- | length acc == 37 = acc
    | length acc == M.size im = acc
    | length acc == 0 = let acc' = acc ++ [setFstCorner tile ns matches] in getRows rowSize (last acc') ns matches im acc'
    | otherwise = let
        srcTile = if (length acc) `mod` rowSize == 0 then acc!!(length acc - rowSize) else tile
        srcSide = if (length acc) `mod` rowSize == 0 then 2 else 1
        tile' = findNextTile srcSide srcTile ns im
        in getRows rowSize tile' ns matches im (acc ++ [tile'])

findNextTile side tile ns im = let
    sideNum = (tileToSideInts $ snd tile)!!side
    m = getMatchTile (fst tile) sideNum ns  -- Get tile m for right side of source
    mStr = im M.! m
    msi = fromJust $ sideNum `elemIndex` (ns M.! m)
    alts1 = [
            (m, flpO . rotR $ mStr),  -- *
            (m, flpO $ mStr),  -- *
            (m, rotR $ mStr),  -- *
            (m, mStr),  -- *
            (m, rotR . rotR . rotR $ mStr),  -- *
            (m, rotR . rotR $ mStr),  -- *
            (m, flpO . rotR . rotR . rotR $ mStr),  -- *
            (m, flpO . rotR . rotR $ mStr)  -- NEVER?
        ]
    alts2 = [
            (m, mStr),  -- *
            (m, rotR . rotR . rotR $ mStr),  -- NEVER?  -- *
            (m, flpO . rotR . rotR $ mStr),  -- NEVER? -- *
            (m, flpO . rotR $ mStr), -- *
            (m, flpO $ mStr),  -- *
            (m, flpO . rotR . rotR . rotR $ mStr),  -- *
            (m, rotR . rotR $ mStr),  -- NEVER?
            (m, rotR $ mStr)  -- *
        ]
    in case side of
        1 -> alts1!!msi
        2 -> alts2!!msi

setFstCorner tile ns matches = let
    config = map (\x ->
            let matchsides = concat . map (\x -> ns M.! x) . nub $ (M.!) matches $ fst tile
            in x `elem` matchsides
        ) (take 4 . tileToSideInts $ snd tile)
    in if config == [False, True, True, False] then tile
        else setFstCorner (fst tile, rotR $ snd tile) ns matches

removeBorders xs = map (\a -> (fst a, map (tail . init) (tail $ init $ snd a))) xs

tileToSideInts tile' = let
    t = head tile'
    b = last tile'
    l = map (\x -> head x) tile'
    r = map (\x -> last x) tile'
    in toBinNum $ [t, r, b, l, reverse t, reverse r, reverse b, reverse l]

toBinNum = map (\x -> binStringToInt $ map (\y -> if y == '#' then '1' else '0') x)

binStringToInt = foldr (\x accum -> (accum * 2) + (if x == '0' then 0 else 1)) 0 . reverse -- Using foldl' instead of foldl for performance

getMatchTile i n = M.foldrWithKey (\k v a -> if a == -1 && i /= k && n `elem` v then k else a) (-1)

rotR = transpose . reverse

flpO = map (reverse)

pretty _ _ [] acc = acc
pretty size sep xs acc = let
    row = take size xs
    acc' = (foldr (\y a ->
                (foldr (\x a -> a ++ sep ++ ((snd x)!!y)) a $ reverse row) ++ "\n"
            ) acc $ reverse [0..(length $ snd $ head row)-1]) ++ (if length sep > 0 then "\n" else "")
    in pretty size sep (drop size xs) acc'