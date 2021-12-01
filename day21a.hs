import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.List (intersect, nub, (\\))

main = do
--     let i = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
-- \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
-- \sqjhc fvjkl (contains soy)\n\
-- \sqjhc mxmxvkd sbzzf (contains fish)"
    i <- readFile "day21.in"

    -- let i' = foldr (\x a ->
    --                 map (\y -> M.insert y x!!0 a) (splitOn ", " x!!1)
    --             ) M.empty $ map (\y -> splitOn " (contains" y) $ lines i
    let i' = map (\y -> splitOn " (contains " $ init y) $ lines i
        i'' = foldr (\x a ->
                    foldr (\y a -> M.insertWith (++) y [words (x!!0)] a) a $ splitOn ", " $ x!!1
                ) M.empty $ map (\y -> splitOn " (contains " $ init y) $ lines i

        allergens = concat . M.elems . process $ possibles i''
        ingredients = concat $ map (\v -> words $ v!!0) i'
        nonAller = (nub $ ingredients) \\ allergens
        numOccur = map (\x -> length $ filter (==x) ingredients) nonAller


    --     j = M.map (\x ->
    --             foldr (\y a -> y `intersect` a) (head x) x
    --         ) i'
    -- print j
    -- print $ remove "dairy" "mxmxvkd" j
    
    -- print i'
    -- print allergens
    -- print ingredients
    -- print nonAller
    -- print numOccur
    print $ sum numOccur
    -- print $ isComplete p
    -- print $ process p
    -- print $ process p

process :: (Eq a) => M.Map a [a] -> M.Map a [a]
process a = if not $ isComplete a then process $ remove a else a

possibles :: (Eq a) => M.Map a [[a]] -> M.Map a[a]
possibles = M.map (\x -> foldr (\y a -> y `intersect` a) (head x) x)

remove :: (Eq a) => M.Map a [a] -> M.Map a [a]
remove a = M.foldrWithKey (\k v a' -> if length v == 1 then remove' k (head v) a' else a') a a

remove' :: (Eq a) => a -> a -> M.Map a [a] -> M.Map a [a]
remove' k v = M.mapWithKey (\k' v' -> if k' /= k then filter (/=v) v' else v')

isComplete :: M.Map a [a] -> Bool
isComplete = M.null . M.filter ((>1) . length)