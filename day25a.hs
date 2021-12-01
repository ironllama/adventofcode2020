main = do
    i <- readFile "day25.in"
    let subject = 7
        i' = map (\x -> read x::Int) $ lines i
        -- card = 5764801
        -- door = 17807724
        -- card = 1965712
        -- door = 19072108
        card = i'!!0
        door = i'!!1
        loopCard = findLoop 0 1 subject card
        loopDoor = findLoop 0 1 subject door
        -- encKey = if loopCard > loopDoor then doLoop loopCard 1 door
        --             else doLoop loopDoor 1 card

    print loopCard
    print loopDoor
    -- print encKey

findLoop :: Int -> Int -> Int -> Int -> Int
findLoop iter acc subject target
    | acc == target = iter
    -- | iter > 100 = -1
    | otherwise = findLoop (iter+1) ((subject * acc) `mod` 20201227) subject target

doLoop :: Int -> Int -> Int -> Int
doLoop iter acc subject
    | iter == 0 = acc
    | otherwise = doLoop (iter-1) ((subject * acc) `mod` 20201227) subject