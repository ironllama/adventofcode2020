import qualified Data.Map.Strict as M
import Data.Maybe

main = do
--     let i = ".#.\n\
-- \..#\n\
-- \###"
    i <- readFile "day17.in"

    let i' = lines i
        allActives = foldr (\y a -> let  -- Create list of actives as x,y,z points
                            y' = i'!!y
                            in foldr (\x b -> let
                                x' = y'!!x
                                in if x' == '#' then (x, y, 0):b else b) a [0..(length y' - 1)]
                        ) [] [0..(length i' - 1)]
        -- allCandidates = getCandidates allActives
        -- newActives = filterCandidates allCandidates allActives
        z = foldr (\x actives -> let
                    oldActives = snd $ head actives
                    allCandidates = getCandidates oldActives
                    newActives = filterCandidates allCandidates oldActives
                    in (length newActives, newActives):actives
                    ) [(length allActives, allActives)] [0..5]

    -- print i'
    -- print allActives
    -- print allCandidates
    -- print newActives
    print . fst $ head z

getCandidates :: [Coords] -> M.Map Coords Int
getCandidates m = foldr (\km' a ->  -- For each active point, contribute 1 to all its neighbors.
                        foldr (\z a ->
                            foldr (\y a ->
                                foldr (\x a ->
                                        if x == 0 && y == 0 && z == 0 then a  -- Igore self.
                                        else let
                                            newPos = ((tFst km') + x, (tSnd km') + y, (tTrd km') + z)  -- New possible point.
                                            curr = fromMaybe (0) $ M.lookup newPos a -- See if already in list.
                                            in M.insert newPos (curr + 1) a  -- Add/create new value.
                                ) a [-1, 0, 1]
                            ) a [-1, 0, 1]
                        ) a [-1, 0, 1]
                    ) M.empty m

filterCandidates :: M.Map Coords Int -> [Coords] -> [Coords]
filterCandidates c o = M.foldrWithKey (\kc' c' a ->  -- If the current point has required active neighbors, activate or inactivate.
                            if kc' `elem` o then (if c' == 2 || c' == 3 then kc':a else a)
                            else (if c' == 3 then kc':a else a)
                        ) [] c

-- No built-ins for triples, so have to make own versions of accessors.
type Coords = (Int, Int, Int)

tFst :: (a, b, c) -> a
tFst (x, _, _) = x

tSnd :: (a, b, c) -> b
tSnd (_, x, _) = x

tTrd :: (a, b, c) -> c
tTrd (_, _, x) = x
