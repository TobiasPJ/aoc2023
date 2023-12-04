module Day4 where
    
    import Distribution.Utils.String (trim)
 
    part1 :: IO ()
    part1  = do
        contents <- readFile "/home/tobias/git/aoc2023/day4/input"
        print (sum (map cardPoints (lines contents)))

    cardPoints :: String -> Int
    cardPoints c = getPoints (length (filter (==True) (getMatches c)))


    getMatchAmount :: String -> Int
    getMatchAmount c = length (filter (==True) (getMatches c))

    getMatches :: String -> [Bool]
    getMatches c = do
        let [_, nums] = splitOn c ':'
        let [winningNums, myNums] = map (\x -> map trim (filter (/="") (splitOn (trim x) ' '))) (splitOn nums '|')
        map (\x -> x `elem` winningNums) myNums


    getPoints :: (Integral b, Num a) => b -> a
    getPoints 0 = 0
    getPoints matches = 1*2^(matches-1)

    splitOn :: String -> Char -> [String]
    splitOn str c = case break (== c) str of
        (a, c : b) -> a : splitOn b c
        (a, "") -> [a]