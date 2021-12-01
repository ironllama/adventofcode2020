import Data.Char

main = do
-- All Bad Passports
--     let input = "eyr:1972 cid:100\n\
-- \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
-- \\n\
-- \iyr:2019\n\
-- \hcl:#602927 eyr:1967 hgt:170cm\n\
-- \ecl:grn pid:012533040 byr:1946\n\
-- \\n\
-- \hcl:dab227 iyr:2012\n\
-- \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
-- \\n\
-- \hgt:59cm ecl:zzz\n\
-- \eyr:2038 hcl:74454a iyr:2023\n\
-- \pid:3556412378 byr:2007"

-- All Good Passports
--     let input = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
-- \hcl:#623a2f\n\
-- \\n\
-- \eyr:2029 ecl:blu cid:129 byr:1989\n\
-- \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
-- \\n\
-- \hcl:#888785\n\
-- \hgt:164cm byr:2001 iyr:2015 cid:88\n\
-- \pid:545766238 ecl:hzl\n\
-- \eyr:2022\n\
-- \\n\
-- \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"

    input <- readFile "day04.in"

    let allCreds = [
                    ("byr", (\x -> (read x :: Int) >= 1920 && (read x :: Int) <= 2002)),
                    ("iyr", (\x -> (read x :: Int) >= 2010 && (read x :: Int) <= 2020)),
                    ("eyr", (\x -> (read x :: Int) >= 2020 && (read x :: Int) <= 2030)),
                    ("hgt", (\x -> let
                                unit = drop (length x - 2) x
                                amt = read (take (length x - 2) x) :: Int
                                in if unit == "cm"
                                    then amt >= 150 && amt <= 193
                                    else if unit == "in"
                                        then amt >= 59 && amt <= 76
                                        else False
                            )
                        ),
                    ("hcl", (\(x:xs) -> length xs == 6 && all isHexDigit xs)),
                    ("ecl", (\x -> x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])),
                    ("pid", (\x -> length x == 9 && all isDigit x))
                    ]
        allLines = lines input
        joinedLines = joinPara allLines
        -- tokenedLines = map splitWords joinedLines
        -- filtered = map (\thisLine -> False `elem` (checkCreds allCreds thisLine)) joinedLines
        -- filtered = [False | thisLine <- joinedLines, False `elem` (checkCreds allCreds thisLine)]
        -- filtered = [allTrues | thisLine <- joinedLines, let allTrues = (checkCreds allCreds thisLine)]
        filtered = map (\thisLine -> checkCreds allCreds thisLine) joinedLines
        badPass = map (\thisPass -> False `elem` thisPass) filtered
        -- filtered = [allTrues | thisLine <- joinedLines, let allTrues = length $ filter (==True) (checkCreds allCreds thisLine), allTrues == 7]
        good = (length joinedLines) - count True badPass
        -- good = length filtered

    -- print joinedLines
    -- print tokenedLines
    -- print filtered
    -- print badPass
    print good

checkCreds :: [(String, String -> Bool)] -> String -> [Bool]
checkCreds allCreds thisLine = map (\thisCred ->
                                        if True `elem` (credGood thisCred (splitWords thisLine))  -- Just need one True or 'fulfilled'
                                            then True
                                            else False
                                        ) allCreds

credGood :: (String, (String -> Bool)) -> [[String]] -> [Bool]
credGood thisCred theseFields = map (\thisField ->
                                        (fst thisCred) == thisField!!0
                                        && (snd thisCred) (thisField!!1)
                                        ) theseFields

splitWords :: String -> [[String]]
splitWords inStr = map (\w -> splitBy ':' w) (words inStr)

joinPara :: [String] -> [String]
joinPara = foldl joinHandler [""]
    where joinHandler inList thisLine
            | thisLine == "" = inList ++ [""]  -- if blank found, add a new string to the list
            | otherwise = (init inList) ++ [(thisLine ++ (if length (last inList) > 0 then " " else "")) ++ (last inList)]  -- append to the last string in list

count :: (Eq a) => a -> [a] -> Int
count c = length . filter (==c)


-- From https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l | otherwise = (c:x):xs