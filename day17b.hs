import qualified Data.Map.Strict as M
import Data.Maybe

main = do
--     let i = ".#.\n\
-- \..#\n\
-- \###"
    i <- readFile "day17.in"

    let i' = lines i
        allActives = foldr (\y a -> let  -- Create list of actives as x,y,z,w points
                            y' = i'!!y
                            in foldr (\x b -> let
                                x' = y'!!x
                                in if x' == '#' then (x, y, 0, 0):b else b) a [0..(length y' - 1)]
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
                                    foldr (\w a ->
                                        if x == 0 && y == 0 && z == 0 && w == 0 then a  -- Igore self.
                                        else let
                                            newPos = ((qFst km') + x, (qSnd km') + y, (qTrd km') + z, (qFrt km') + w)  -- New possible poinq.
                                            curr = fromMaybe (0) $ M.lookup newPos a -- See if already in list.
                                            in M.insert newPos (curr + 1) a  -- Add/create new value.
                                    ) a [-1, 0, 1]
                                ) a [-1, 0, 1]
                            ) a [-1, 0, 1]
                        ) a [-1, 0, 1]
                    ) M.empty m

filterCandidates :: M.Map Coords Int -> [Coords] -> [Coords]
filterCandidates c o = M.foldrWithKey (\kc' c' a ->  -- If the current point has required active neighbors, activate or inactivate.
                            if kc' `elem` o then (if c' == 2 || c' == 3 then kc':a else a)
                            else (if c' == 3 then kc':a else a)
                        ) [] c

-- No built-ins for quads, so have to make own versions of accessors.
type Coords = (Int, Int, Int, Int)

qFst :: (a, b, c, d) -> a
qFst (x, _, _, _) = x

qSnd :: (a, b, c, d) -> b
qSnd (_, x, _, _) = x

qTrd :: (a, b, c, d) -> c
qTrd (_, _, x, _) = x

qFrt :: (a, b, c, d) -> d
qFrt (_, _, _, x) = x