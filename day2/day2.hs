module Day2 where

import Data.Char (isDigit, ord)
import Debug.Trace (trace)
import Distribution.Utils.String (trim)

part1 :: IO ()
part1 = do
  contents <- readFile "/Users/tobiaspetterssonjensen/Desktop/git/aoc2023/day2/input"
  let games = map (\g -> checkGame g 14 13 12) (lines contents)
  let possibleGames = map snd (filter fst games)
  print (sum possibleGames)

part2 :: IO ()
part2 = do
  contents <- readFile "/Users/tobiaspetterssonjensen/Desktop/git/aoc2023/day2/input"
  print (sum (map gamePower (lines contents)))

rejectNonPossibleGames :: [(Bool, Int)] -> [Int]
rejectNonPossibleGames [] = []
rejectNonPossibleGames ((False, _) : r) = rejectNonPossibleGames r
rejectNonPossibleGames ((True, gameId) : r) = gameId : rejectNonPossibleGames r

gamePower :: String -> Int
gamePower g = do
  let [_, game] = splitOn g ':'
  let sets = map trim (splitOn (trim game) ';')
  let amounts = concatMap getSetAmounts sets
  let bAmounts = map snd (filter (\(colour, a) -> colour == "blue") amounts)
  let gAmounts = map snd (filter (\(colour, a) -> colour == "green") amounts)
  let rAmounts = map snd (filter (\(colour, a) -> colour == "red") amounts)
  maximum bAmounts * maximum gAmounts * maximum rAmounts

getSetAmounts :: String -> [(String, Int)]
getSetAmounts s = do
  let amounts = map trim (splitOn s ',')
  map (getAmount . (`splitOn` ' ')) amounts

checkGame :: String -> Int -> Int -> Int -> (Bool, Int)
checkGame g mb mg mr = do
  let gId = getGameId g
  let [_, sets] = splitOn g ':'
  let setChecks = all ((\x -> checkSet x mb mg mr) . trim) (splitOn sets ';')
  (setChecks, strToInt gId)

checkSet :: String -> Int -> Int -> Int -> Bool
checkSet s mb mg mr = do
  let cAmounts = map trim (splitOn s ',')
  all ((\x -> checkAmount x mb mg mr) . (`splitOn` ' ')) cAmounts

checkAmount :: [String] -> Int -> Int -> Int -> Bool
checkAmount [amount, "blue"] mb _ _ = strToInt amount <= mb
checkAmount [amount, "green"] _ mg _ = strToInt amount <= mg
checkAmount [amount, "red"] _ _ mr = strToInt amount <= mr
checkAmount [_, _] _ _ _ = False

getAmount :: [String] -> (String, Int)
getAmount [amount, colour] = (colour, strToInt amount)

getGameId :: String -> String
getGameId (':' : _) = []
getGameId (h : r)
  | isDigit h = h : getGameId r
  | otherwise = getGameId r

strToInt :: String -> Int
strToInt str = do
  let l = length str
  parseIntStr str l

parseIntStr :: (Integral t) => String -> t -> Int
parseIntStr [] _ = 0
parseIntStr (h : r) pos = charToInt h * 10 ^ (pos - 1) + parseIntStr r (pos - 1)

charToInt :: Char -> Int
charToInt c = ord c - 48

splitOn :: String -> Char -> [String]
splitOn str c = case break (== c) str of
  (a, c : b) -> a : splitOn b c
  (a, "") -> [a]