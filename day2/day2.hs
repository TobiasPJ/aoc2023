module Day2 where

import Data.Char (isDigit, ord)
import Debug.Trace (trace)
import Distribution.Utils.String (trim)

part1 :: IO ()
part1 = do
  contents <- readFile "/Users/tobiaspetterssonjensen/Desktop/git/aoc2023/day2/input"
  print (sum (rejectNonPossibleGames (map (\x -> checkGame x 14 13 12) (lines contents))))

rejectNonPossibleGames :: [(Bool, Int)] -> [Int]
rejectNonPossibleGames [] = []
rejectNonPossibleGames ((False, _) : r) = rejectNonPossibleGames r
rejectNonPossibleGames ((True, gameId) : r) = gameId : rejectNonPossibleGames r

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
checkAmount [amount, 'b' : 'l' : 'u' : 'e' : _] mb _ _ = strToInt amount <= mb
checkAmount [amount, 'g' : 'r' : 'e' : 'e' : 'n' : _] _ mg _ = strToInt amount <= mg
checkAmount [amount, 'r' : 'e' : 'd' : _] _ _ mr = strToInt amount <= mr
checkAmount [_, _] _ _ _ = False

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