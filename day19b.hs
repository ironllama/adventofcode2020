import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as M
import Text.Regex.TDFA

main = do
--     let i = "42: 9 14 | 10 1\n\
-- \9: 14 27 | 1 26\n\
-- \10: 23 14 | 28 1\n\
-- \1: \"a\"\n\
-- \11: 42 31\n\
-- \5: 1 14 | 15 1\n\
-- \19: 14 1 | 14 14\n\
-- \12: 24 14 | 19 1\n\
-- \16: 15 1 | 14 14\n\
-- \31: 14 17 | 1 13\n\
-- \6: 14 14 | 1 14\n\
-- \2: 1 24 | 14 4\n\
-- \0: 8 11\n\
-- \13: 14 3 | 1 12\n\
-- \15: 1 | 14\n\
-- \17: 14 2 | 1 7\n\
-- \23: 25 1 | 22 14\n\
-- \28: 16 1\n\
-- \4: 1 1\n\
-- \20: 14 14 | 1 15\n\
-- \3: 5 14 | 16 1\n\
-- \27: 1 6 | 14 18\n\
-- \14: \"b\"\n\
-- \21: 14 1 | 1 14\n\
-- \25: 1 1 | 1 14\n\
-- \22: 14 14\n\
-- \8: 42\n\
-- \26: 14 22 | 1 20\n\
-- \18: 15 15\n\
-- \7: 14 5 | 1 21\n\
-- \24: 14 1\n\
-- \\n\
-- \abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
-- \bbabbbbaabaabba\n\
-- \babbbbaabbbbbabbbbbbaabaaabaaa\n\
-- \aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
-- \bbbbbbbaaaabbbbaaabbabaaa\n\
-- \bbbababbbbaaaaaaaabbababaaababaabab\n\
-- \ababaaaaaabaaab\n\
-- \ababaaaaabbbaba\n\
-- \baabbaaaabbaaaababbaababb\n\
-- \abbbbabbbbaaaababbbbbbaaaababb\n\
-- \aaaaabbaabaaaaababaa\n\
-- \aaaabbaaaabbaaa\n\
-- \aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
-- \babaaabbbaaabaababbaabababaaab\n\
-- \aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
    i <- readFile "day19.in"
    -- i <- readFile "day19b.in"  -- Originally, just isolated the first round of matches, but it was still more than half, and wasn't the bottleneck.

    let i' = lines i
        spl = fromMaybe (0) $ "" `elemIndex` i'
        inp = (take spl i', drop (spl + 1) i')
        -- inp = (0, i')

        rules = lrules (fst inp) M.empty

        -- Since "0: 8 11" means that we just need to process 8 and 11
        -- and "8: 42" is now "8: 42 | 42 8", we have one or more 42
        -- and "11: 42 31" is now "11: 42 31 | 42 11 31", we have 42 recursive 11's and then 31 -- I just guessed at the number of recursions.
        -- patt = "\\b" ++ lpattern 0 rules ++ "\\b"
        -- patt42 = "\\b" ++ lpattern 42 rules ++ "\\b"
        -- patt31 = "\\b" ++ lpattern 31 rules ++ "\\b"
        patt42 = lpattern 42 rules
        patt31 = lpattern 31 rules
        -- patt42 = "(((a(a(((bb|aa)b|(ab|aa)a)a|((ba|aa)b|(bb|aa)a)b)|b((a(ab|ba)|b(bb|ab))b|((bb|ba)a|(ab|ba)b)a))|b(b((b(a|b)(a|b)|a(b(a|b)|aa))a|(((a|b)a|ab)b|(b(a|b)|aa)a)b)|a(b(aba|b(ba|aa))|abbb)))a|(b(a((((a|b)a|ab)b|aaa)b|(baa|((a|b)b|ba)b)a)|b(((b(a|b)|aa)a|(ab|aa)b)a|(a(b(a|b)|aa)|b(ba|aa))b))|a((((b(a|b)|aa)b|(ba|aa)a)b|(bbb|a(bb|aa))a)a|(a(b(bb|ab)|a(b(a|b)|aa))|b(bbb|a((a|b)b|ba)))b))b)a|((((b(bab|bba)|a(aba|b((a|b)b|ba)))a|(((b(a|b)|aa)b|baa)b|(b(bb|aa)|a(bb|ab))a)b)b|(a(b(((a|b)b|ba)b|(ab|ba)a)|a(a(ab|aa)|b(bb|ba)))|b(a((a(a|b)|bb)b|(ba|aa)a)|b((bb|ab)a|(a|b)(a|b)b)))a)a|(b(b((baa|(bb|ab)b)b|(a(a|b)(a|b)|b(ab|ba))a)|a(a(baa|aab)|b(a|b)(bb|ab)))|a(b(abbb|b((bb|ab)b|bba))|a((b((a|b)a|ab)|aba)b|((ab|aa)a|aab)a)))b)b)"
        -- patt31 = "(a((((((ab|ba)a|(bb|aa)b)a|((ab|ba)a|aab)b)a|(((b(a|b)|aa)b|(ba|aa)a)b|(baa|a(a(a|b)|bb))a)b)a|(a(a(bba|(a(a|b)|bb)b)|b(a(ba|aa)|b(b(a|b)|aa)))|b((a((a|b)a|ab)|b(ba|aa))b|((a|b)(a|b)a|((a|b)a|ab)b)a))b)b|(a(a(b(a(ab|ba)|b(ba|aa))|abbb)|b((a((a|b)a|ab)|b(ab|ba))b|(((a|b)a|ab)a|(bb|aa)b)a))|b((((bb|aa)b|(ab|aa)a)a|(a(bb|aa)|b(ba|aa))b)a|(a(aab|(ba|aa)a)|b(((a|b)a|ab)b|(ab|aa)a))b))a)|b((a(b(((b(a|b)|aa)b|baa)b|(a(a(a|b)|bb)|b(bb|ab))a)|a(((a|b)(a|b)a|((a|b)a|ab)b)a|(a(ba|aa)|b(ab|aa))b))|b((a(bab|aba)|bbbb)a|((aba|b(bb|ab))a|(aab|b(bb|ab))b)b))a|((a(((ab|aa)b|aba)b|((b(a|b)|aa)a|(a|b)(a|b)b)a)|b((baa|bbb)a|(a((a|b)a|ab)|b(ab|ba))b))b|((((ab|ba)a|bab)b|(b(bb|aa)|a(a|b)(a|b))a)b|(((bb|ba)a|(ab|aa)b)a|abab)a)a)b))"
        -- patt = "\\b" ++ patt42 ++ "+" ++ patt42 ++ "+" ++ patt31 ++ "+\\b"
        -- Manually crank through the possibilities. Seems like it gets no more than 3 deep for the second pattern.
        -- patt = "\\b" ++ patt42 ++ "+(" ++ patt42 ++ patt31 ++ "|" ++ patt42 ++ patt42 ++ patt31 ++ patt31 ++ "|" ++ patt42 ++ patt42 ++ patt42 ++ patt31 ++ patt31 ++ patt31 ++ "|" ++ patt42 ++ patt42 ++ patt42 ++ patt42 ++ patt31 ++ patt31 ++ patt31 ++ patt31 ++ ")\\b"
        patt = "\\b" ++ patt42 ++ "+(" ++ patt42 ++ patt31 ++ "|" ++ patt42 ++ patt42 ++ patt31 ++ patt31 ++ "|" ++ patt42 ++ patt42 ++ patt42 ++ patt31 ++ patt31 ++ patt31 ++ ")\\b"
        -- patt = "\\b" ++ patt42 ++ "+(" ++ patt42 ++ patt31 ++ "|" ++ patt42 ++ patt42 ++ patt31 ++ patt31 ++ ")\\b"
        z = foldr (\x a -> if x =~ patt::Bool then x:a else a) [] $ snd inp
        -- z = foldr (\x a -> if x =~ patt::Bool then x:a else a) [] $ snd inp

    -- print i'
    -- print inp
    -- print rules
    -- print patt
    -- print $ foldr (\x a -> if x =~ patt::Bool then a+1 else a) 0 $ snd inp
    -- mapM_ print $ z
    print $ length z

lrules :: [String] -> M.IntMap [String] -> M.IntMap [String]
lrules [] acc = acc
lrules (x:xs) acc = let
    parts = words x
    num = read (init $ head parts)::Int
    val = case num of
            -- test data
            -- 8 -> ["((b(a(bb|ab)|b(a|b)(a|b))|a(bbb|a(bb|a(a|b))))b|(((aa|ab)a|bbb)b|((a|b)a|bb)aa)a)+"]
            -- 11 -> ["((b(a(bb|ab)|b(a|b)(a|b))|a(bbb|a(bb|a(a|b))))b|(((aa|ab)a|bbb)b|((a|b)a|bb)aa)a)+(b(b(aba|baa)|a(b(ab|(a|b)a)|a(ba|ab)))|a(b((ab|(a|b)a)b|((a|b)a|bb)a)|a(bab|(ba|bb)a)))+"]
            -- 11 -> ["(b(b(aba|baa)|a(b(ab|(a|b)a)|a(ba|ab)))|a(b((ab|(a|b)a)b|((a|b)a|bb)a)|a(bab|(ba|bb)a)))+"]
            -- 8 -> ["(((a(a(((bb|aa)b|(ab|aa)a)a|((ba|aa)b|(bb|aa)a)b)|b((a(ab|ba)|b(bb|ab))b|((bb|ba)a|(ab|ba)b)a))|b(b((b(a|b)(a|b)|a(b(a|b)|aa))a|(((a|b)a|ab)b|(b(a|b)|aa)a)b)|a(b(aba|b(ba|aa))|abbb)))a|(b(a((((a|b)a|ab)b|aaa)b|(baa|((a|b)b|ba)b)a)|b(((b(a|b)|aa)a|(ab|aa)b)a|(a(b(a|b)|aa)|b(ba|aa))b))|a((((b(a|b)|aa)b|(ba|aa)a)b|(bbb|a(bb|aa))a)a|(a(b(bb|ab)|a(b(a|b)|aa))|b(bbb|a((a|b)b|ba)))b))b)a|((((b(bab|bba)|a(aba|b((a|b)b|ba)))a|(((b(a|b)|aa)b|baa)b|(b(bb|aa)|a(bb|ab))a)b)b|(a(b(((a|b)b|ba)b|(ab|ba)a)|a(a(ab|aa)|b(bb|ba)))|b(a((a(a|b)|bb)b|(ba|aa)a)|b((bb|ab)a|(a|b)(a|b)b)))a)a|(b(b((baa|(bb|ab)b)b|(a(a|b)(a|b)|b(ab|ba))a)|a(a(baa|aab)|b(a|b)(bb|ab)))|a(b(abbb|b((bb|ab)b|bba))|a((b((a|b)a|ab)|aba)b|((ab|aa)a|aab)a)))b)b)+"]
            -- 11 -> ["(((a(a(((bb|aa)b|(ab|aa)a)a|((ba|aa)b|(bb|aa)a)b)|b((a(ab|ba)|b(bb|ab))b|((bb|ba)a|(ab|ba)b)a))|b(b((b(a|b)(a|b)|a(b(a|b)|aa))a|(((a|b)a|ab)b|(b(a|b)|aa)a)b)|a(b(aba|b(ba|aa))|abbb)))a|(b(a((((a|b)a|ab)b|aaa)b|(baa|((a|b)b|ba)b)a)|b(((b(a|b)|aa)a|(ab|aa)b)a|(a(b(a|b)|aa)|b(ba|aa))b))|a((((b(a|b)|aa)b|(ba|aa)a)b|(bbb|a(bb|aa))a)a|(a(b(bb|ab)|a(b(a|b)|aa))|b(bbb|a((a|b)b|ba)))b))b)a|((((b(bab|bba)|a(aba|b((a|b)b|ba)))a|(((b(a|b)|aa)b|baa)b|(b(bb|aa)|a(bb|ab))a)b)b|(a(b(((a|b)b|ba)b|(ab|ba)a)|a(a(ab|aa)|b(bb|ba)))|b(a((a(a|b)|bb)b|(ba|aa)a)|b((bb|ab)a|(a|b)(a|b)b)))a)a|(b(b((baa|(bb|ab)b)b|(a(a|b)(a|b)|b(ab|ba))a)|a(a(baa|aab)|b(a|b)(bb|ab)))|a(b(abbb|b((bb|ab)b|bba))|a((b((a|b)a|ab)|aba)b|((ab|aa)a|aab)a)))b)b)+(a((((((ab|ba)a|(bb|aa)b)a|((ab|ba)a|aab)b)a|(((b(a|b)|aa)b|(ba|aa)a)b|(baa|a(a(a|b)|bb))a)b)a|(a(a(bba|(a(a|b)|bb)b)|b(a(ba|aa)|b(b(a|b)|aa)))|b((a((a|b)a|ab)|b(ba|aa))b|((a|b)(a|b)a|((a|b)a|ab)b)a))b)b|(a(a(b(a(ab|ba)|b(ba|aa))|abbb)|b((a((a|b)a|ab)|b(ab|ba))b|(((a|b)a|ab)a|(bb|aa)b)a))|b((((bb|aa)b|(ab|aa)a)a|(a(bb|aa)|b(ba|aa))b)a|(a(aab|(ba|aa)a)|b(((a|b)a|ab)b|(ab|aa)a))b))a)|b((a(b(((b(a|b)|aa)b|baa)b|(a(a(a|b)|bb)|b(bb|ab))a)|a(((a|b)(a|b)a|((a|b)a|ab)b)a|(a(ba|aa)|b(ab|aa))b))|b((a(bab|aba)|bbbb)a|((aba|b(bb|ab))a|(aab|b(bb|ab))b)b))a|((a(((ab|aa)b|aba)b|((b(a|b)|aa)a|(a|b)(a|b)b)a)|b((baa|bbb)a|(a((a|b)a|ab)|b(ab|ba))b))b|((((ab|ba)a|bab)b|(b(bb|aa)|a(a|b)(a|b))a)b|(((bb|ba)a|(ab|aa)b)a|abab)a)a)b))+"]
            -- 11 -> ["(a((((((ab|ba)a|(bb|aa)b)a|((ab|ba)a|aab)b)a|(((b(a|b)|aa)b|(ba|aa)a)b|(baa|a(a(a|b)|bb))a)b)a|(a(a(bba|(a(a|b)|bb)b)|b(a(ba|aa)|b(b(a|b)|aa)))|b((a((a|b)a|ab)|b(ba|aa))b|((a|b)(a|b)a|((a|b)a|ab)b)a))b)b|(a(a(b(a(ab|ba)|b(ba|aa))|abbb)|b((a((a|b)a|ab)|b(ab|ba))b|(((a|b)a|ab)a|(bb|aa)b)a))|b((((bb|aa)b|(ab|aa)a)a|(a(bb|aa)|b(ba|aa))b)a|(a(aab|(ba|aa)a)|b(((a|b)a|ab)b|(ab|aa)a))b))a)|b((a(b(((b(a|b)|aa)b|baa)b|(a(a(a|b)|bb)|b(bb|ab))a)|a(((a|b)(a|b)a|((a|b)a|ab)b)a|(a(ba|aa)|b(ab|aa))b))|b((a(bab|aba)|bbbb)a|((aba|b(bb|ab))a|(aab|b(bb|ab))b)b))a|((a(((ab|aa)b|aba)b|((b(a|b)|aa)a|(a|b)(a|b)b)a)|b((baa|bbb)a|(a((a|b)a|ab)|b(ab|ba))b))b|((((ab|ba)a|bab)b|(b(bb|aa)|a(a|b)(a|b))a)b|(((bb|ba)a|(ab|aa)b)a|abab)a)a)b))+"]
            _ -> if length (tail parts) > 1 then tail parts else [filter (/='"') $ last parts]
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