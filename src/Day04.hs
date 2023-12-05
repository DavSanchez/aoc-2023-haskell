module Day04 (day04) where

import Data.List (intersect, iterate')
import Paths_aoc2023 (getDataFileName)

day04 :: IO ()
day04 = do
  inputLines <- lines <$> (getDataFileName "day04-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> (show . sum . map (points . solve01)) inputLines
  putStrLn $ "Result 2: " <> (show . length . solve02) inputLines

_example :: String
_example = "Card   1: 13  4 61 82 80 41 31 53 50  2 | 38 89 26 79 94 50  2 74 31 92 80 41 13 97 61 82 68 45 64 39  4 53 90 84 54"

_example2 :: [String]
_example2 =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

solve02 :: [String] -> [Int]
solve02 = cardLoop

-- Manual implementation. See below for alternative based on `iterate`.
-- >>> length $ cardLoop _example2
-- 30
_cardLoop :: [String] -> [Int]
_cardLoop strs = go [] (indexed strs)
  where
    go acc [] = acc
    go acc xs' = go (acc ++ map fst xs') (obtainedCards strs xs')

-- >>> length $ cardLoop' _example2
-- 30
cardLoop :: [String] -> [Int]
cardLoop strs = (map fst . concat . takeWhile (not . null) . iterate' (obtainedCards strs) . indexed) strs

indexed :: [String] -> [(Int, String)]
indexed = zip [0 ..]

-- >>> obtainedCards _example2 (indexed _example2)
-- [(1,"Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"),(2,"Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"),(3,"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"),(4,"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"),(2,"Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"),(3,"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"),(3,"Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"),(4,"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"),(4,"Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36")]
obtainedCards :: [String] -> [(Int, String)] -> [(Int, String)]
obtainedCards strs = concatMap go
  where
    go (i', str') = map (indexed strs !!) (indexes (i', str'))
    indexes (i, str) = let j = solve01 str in if j < 1 then [] else [i + 1 .. i + j]

-- >>> solve01 $ head _example2
-- 8
solve01 :: String -> Int
solve01 str = length (winning `intersect` mine)
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
