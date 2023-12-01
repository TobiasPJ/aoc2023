module Part1 where

    import Data.Char (ord)

    part1 :: IO ()
    part1 = do
        contents <- readFile "/home/tobias/git/aoc2023/day1/input.txt"
        print (sum (getNums (words contents)))

    part2 :: IO ()
    part2 = do
        contents <- readFile "/home/tobias/git/aoc2023/day1/input.txt"
        print (sum (getNums (map replaceDigitWordInString (words contents))))


    getNums :: [String] -> [Int]
    getNums [] = []
    getNums (h:r) = [getTwoDigNumsFromString h] ++ getNums r

    replaceDigitWordInString :: String -> String
    -- replaceDigitWordInString ('z':'e':'r':'o':r) = ['0'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('o':'n':'e':'i':'g':'h':'t':r) = ['1', '8'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('o':'n':'e':r) = ['1'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('t':'w':'o':'n':'e':r) = ['2', '1'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('t':'w':'o':r) = ['2'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('t':'h':'r':'e':'e':'i':'g':'h':'t':r) = ['3', '8'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('t':'h':'r':'e':'e':r) = ['3'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('f':'o':'u':'r':r) = ['4'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('f':'i':'v':'e':'i':'g':'h':'t':r) = ['5', '8'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('f':'i':'v':'e':r) = ['5'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('s':'i':'x':r) = ['6'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('s':'e':'v':'e':'n':'i':'n':'e':r) = ['7', '9'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('s':'e':'v':'e':'n':r) = ['7'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('e':'i':'g':'h':'t':'w':'o':r) = ['8', '2'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('e':'i':'g':'h':'t':r) = ['8'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('n':'i':'n':'e':'i':'g':'h':'t':r) = ['9', '8'] ++ replaceDigitWordInString r
    replaceDigitWordInString ('n':'i':'n':'e':r) = ['9'] ++ replaceDigitWordInString r
    replaceDigitWordInString [] = []
    replaceDigitWordInString (h:r) = [h] ++ replaceDigitWordInString r
     
    getTwoDigNumsFromString :: String -> Int
    getTwoDigNumsFromString s = do 
        let n = extractNums s
        (getFirst n ) * 10 + (getLast n)

    extractNums :: String -> [Int]
    extractNums [] = [] 
    extractNums (h:r) 
        | h >= '0' && h <= '9' = [charToInt h] ++ extractNums r
        | otherwise = extractNums r

    charToInt :: Char -> Int
    charToInt c = ord c - 48

    getFirst :: [Int] -> Int
    getFirst [h] = h
    getFirst (h:_) = h


    getLast :: [Int] -> Int
    getLast [h] = h
    getLast (_:r) = getLast(r)

