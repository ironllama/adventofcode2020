import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as M
import Text.Regex.TDFA

main = do
--     let i = "0: 4 1 5\n\
-- \1: 2 3 | 3 2\n\
-- \2: 4 4 | 5 5\n\
-- \3: 4 5 | 5 4\n\
-- \4: \"a\"\n\
-- \5: \"b\"\n\
-- \\n\
-- \ababbb\n\
-- \bababa\n\
-- \abbbab\n\
-- \aaabbb\n\
-- \aaaabbb"
    i <- readFile "day19.in"

    let i' = lines i
        spl = fromMaybe (0) $ "" `elemIndex` i'
        inp = (take spl i', drop (spl + 1) i')

        rules = lrules (fst inp) M.empty
        patt = "\\b" ++ lpattern 0 rules ++ "\\b"
        -- pt = "\b" ++ (lrules $ fst inp []) ++ "\b"

    -- print i'
    -- print inp
    -- print rules
    -- print patt
    print $ foldr (\x a -> if x =~ patt::Bool then a+1 else a) 0 $ snd inp

lrules :: [String] -> M.IntMap [String] -> M.IntMap [String]
lrules [] acc = acc
lrules (x:xs) acc = let
    parts = words x
    num = read (init $ head parts)::Int
    val = if length (tail parts) > 1 then tail parts else [filter (/='"') $ last parts]
    in lrules xs (M.insert num val acc)

lpattern :: Int -> M.IntMap [String] -> String
lpattern x rs = let
    new = foldr f [] (rs M.! x)
            where f x a
                    | x == "|" = x ++ a
                    | isNumber x = (lpattern (read x::Int) rs) ++ a
                    | otherwise = x ++ a
    in if "|" `elem` (rs M.! x) then "(" ++ new ++ ")" else new

isNumber :: String -> Bool
isNumber str = case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False