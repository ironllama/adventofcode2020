import qualified Data.Map.Strict as M
import Data.Maybe

main = do
    i <- readFile "day17.in"

    let i' = lines i
        a = foldr (\y a ->
                    foldr (\x b -> if ((i'!!y)!!x) == '#' then (x, y, 0, 0):b else b) a [0..(length (i'!!y) - 1)]
                ) [] [0..(length i' - 1)]
        z = foldr (\x la -> let
                    o = snd $ head la
                    c = fg o
                    n = ff c o
                    in (length n, n):la
                    ) [(length a, a)] [0..5]

    print . fst $ head z

fg :: [Cds] -> M.Map Cds Int
fg m = foldr (\km' a ->
                        foldr (\z a ->
                            foldr (\y a ->
                                foldr (\x a ->
                                    foldr (\w a ->
                                        if x == 0 && y == 0 && z == 0 && w == 0 then a
                                        else let
                                            newPos = ((qFst km') + x, (qSnd km') + y, (qTrd km') + z, (qFrt km') + w)
                                            curr = fromMaybe (0) $ M.lookup newPos a
                                            in M.insert newPos (curr + 1) a
                                    ) a [-1, 0, 1]
                                ) a [-1, 0, 1]
                            ) a [-1, 0, 1]
                        ) a [-1, 0, 1]
                    ) M.empty m

ff :: M.Map Cds Int -> [Cds] -> [Cds]
ff c o = M.foldrWithKey (\kc' c' a ->
                            if kc' `elem` o then (if c' == 2 || c' == 3 then kc':a else a)
                            else (if c' == 3 then kc':a else a)
                        ) [] c

type Cds = (Int, Int, Int, Int)

qFst :: (a, b, c, d) -> a
qFst (x, _, _, _) = x

qSnd :: (a, b, c, d) -> b
qSnd (_, x, _, _) = x

qTrd :: (a, b, c, d) -> c
qTrd (_, _, x, _) = x

qFrt :: (a, b, c, d) -> d
qFrt (_, _, _, x) = x