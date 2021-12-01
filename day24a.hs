import Data.List

main = do
    -- let i = "esew"
    -- let i = "nwwswee"
    -- let i = "sesenwnenenewseesww"
    -- let i = "sesenw"
    -- let i = "sesenwnenenewseesww"
    -- let i = "sesenwnenenewseeswwswswwnenewsewsw"
--     let i = "sesenwnenenewseeswwswswwnenewsewsw\n\
-- \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
-- \seswneswswsenwwnwse\n\
-- \nwnwneseeswswnenewneswwnewseswneseene\n\
-- \swweswneswnenwsewnwneneseenw\n\
-- \eesenwseswswnenwswnwnwsewwnwsene\n\
-- \sewnenenenesenwsewnenwwwse\n\
-- \wenwwweseeeweswwwnwwe\n\
-- \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
-- \neeswseenwwswnwswswnw\n\
-- \nenwswwsewswnenenewsenwsenwnesesenew\n\
-- \enewnwewneswsewnwswenweswnenwsenwsw\n\
-- \sweneswneswneneenwnewenewwneswswnese\n\
-- \swwesenesewenwneswnwwneseswwne\n\
-- \enesenwswwswneneswsenwnewswseenwsese\n\
-- \wnwnesenesenenwwnenwsewesewsesesew\n\
-- \nenewswnwewswnenesenwnesewesw\n\
-- \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
-- \neswnwewnwnwseenwseesewsenwsweewe\n\
-- \wseweeenwnesenwwwswnew"
    i <- readFile "day24.in"

    let i' = lines i
        p = foldr (\v a -> let n = initTiles v ' ' (0,0) in if n `elem` a then delete n a else n:a) [] i'

    -- print $ initTiles i ' ' (0, 0)
    -- print $ length i'
    -- print p
    print $ length p

initTiles :: String -> Char -> (Int, Int) -> (Int, Int)
initTiles [] _ a = a
initTiles (x:xs) p a = if x == 'n' || x == 's' then initTiles xs x a
                        else let
                            eveny = (snd a) `mod` 2 == 0
                            ew = if eveny && (p == 'n' || p == 's') then (if x == 'e' then 0 else -1)
                                    else if (p == 'n' || p == 's') then (if x == 'e' then 1 else 0)
                                    else if x == 'e' then 1 else -1
                            ns = case p of
                                    'n' -> 1
                                    's' -> -1
                                    _ -> 0
                            in initTiles xs ' ' ((fst a) + ew, (snd a) + ns)