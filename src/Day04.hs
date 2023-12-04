module Day04 (day04) where

import Data.List (intersect)
import Paths_aoc2023 (getDataFileName)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> (show . sum . map solve01) inputLines
  putStrLn "TODO: implement Day 04"

_example :: String
_example = "Card   1: 13  4 61 82 80 41 31 53 50  2 | 38 89 26 79 94 50  2 74 31 92 80 41 13 97 61 82 68 45 64 39  4 53 90 84 54"

solve01 :: [Char] -> Int
solve01 str = (points . length) (winning `intersect` mine)
  where
    game = cardGame str
    winning = winningNums game
    mine = cardsIHave game

-- >>> winningNums $ cardGame _example
-- [13,4,61,82,80,41,31,53,50,2]
winningNums :: [Char] -> [Int]
winningNums = map read . words . takeWhile (/= '|')

-- >>> cardsIHave $ cardGame _example
-- [38,89,26,79,94,50,2,74,31,92,80,41,13,97,61,82,68,45,64,39,4,53,90,84,54]
cardsIHave :: [Char] -> [Int]
cardsIHave = map read . words . drop 1 . dropWhile (/= '|')

-- >>> cardGame _example
-- " 13  4 61 82 80 41 31 53 50  2 | 38 89 26 79 94 50  2 74 31 92 80 41 13 97 61 82 68 45 64 39  4 53 90 84 54"
cardGame :: String -> String
cardGame = drop 1 . dropWhile (/= ':')

points :: Int -> Int
points n | n < 1 = 0
points n = 2 ^ (n - 1)
