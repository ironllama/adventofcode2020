import Data.List

main = do
--     let i = "16\n\
-- \10\n\
-- \15\n\
-- \5\n\
-- \1\n\
-- \11\n\
-- \7\n\
-- \19\n\
-- \6\n\
-- \12\n\
-- \4"
--     let i = "28\n\
-- \33\n\
-- \18\n\
-- \42\n\
-- \31\n\
-- \14\n\
-- \46\n\
-- \20\n\
-- \48\n\
-- \47\n\
-- \24\n\
-- \23\n\
-- \49\n\
-- \45\n\
-- \19\n\
-- \38\n\
-- \39\n\
-- \11\n\
-- \1\n\
-- \32\n\
-- \25\n\
-- \35\n\
-- \8\n\
-- \17\n\
-- \7\n\
-- \9\n\
-- \4\n\
-- \2\n\
-- \34\n\
-- \10\n\
-- \3"
    i <- readFile "day10.in"

    let i' = sort $ map (\x -> read x::Int) $ lines i  -- get list of numbers
        i'' = (0:i') ++ [(last i') + 3]  -- add the socket (0) and device (largest + 3)
        zi = zip i'' (drop 1 i'')  -- zip up with next value so that we can use a fold
        j' = foldr (\x a-> if (snd x - fst x) == 3 then (fst a, (snd a) + 1) else ((fst a) + 1, snd a)) (0, 0) zi
        z = fst j' * snd j'

    -- print i''
    -- print zi
    -- print j'
    print z