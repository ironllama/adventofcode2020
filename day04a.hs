main = do
--     let input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
-- \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
-- \\n\
-- \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
-- \hcl:#cfa07d byr:1929\n\
-- \\n\
-- \hcl:#ae17e1 iyr:2013\n\
-- \eyr:2024\n\
-- \ecl:brn pid:760753108 byr:1931\n\
-- \hgt:179cm\n\
-- \\n\
-- \hcl:#cfa07d eyr:2025 pid:166559648\n\
-- \iyr:2011 ecl:brn hgt:59in"
    input <- readFile "day04.in"

    -- let allLines = splitBy '\n' input
    let allCreds = ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"]
        allLines = lines input
        joinedLines = joinPara allLines
    -- print allLines
        -- filtered = map (\thisLine -> False `elem` (checkCreds allCreds thisLine)) joinedLines
        -- filtered = [False | thisLine <- joinedLines, False `elem` (checkCreds allCreds thisLine)]
        -- filtered = length [False | thisLine <- joinedLines, False `elem` (checkCreds allCreds thisLine)]
        -- good = (length joinedLines) - filtered
        filtered = [allTrues | thisLine <- joinedLines, let allTrues = length $ filter (==True) (checkCreds allCreds thisLine), allTrues == 7]
        good = length filtered

    print good

checkCreds :: [String] -> String -> [Bool]
checkCreds allCreds thisLine = foldr
    (\thisCred accum ->
        if thisCred `substring` thisLine
            then True:accum
            else False:accum
    ) [] allCreds

joinPara :: [String] -> [String]
joinPara = foldr joinHandler [[]]
    where joinHandler inLine inList@(x:xs)
            | inLine == "" = []:inList  -- if blank found, add a new list to the accumulator
            | otherwise = (inLine ++ " " ++ x):xs  -- add to the first list in the accumulator


-- From https://stackoverflow.com/questions/30588221/check-a-string-if-it-contains-a-given-substring-and-return-boolean
substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix (_:_) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys