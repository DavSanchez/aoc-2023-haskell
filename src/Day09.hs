module Day09 (day09) where

import Paths_aoc2023 (getDataFileName)

day09 :: IO ()
day09 = do
  inputLines <- lines <$> (getDataFileName "day09-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> (show . sum . map (solve1 . parse)) inputLines
  putStrLn $ "Result 2: " <> (show . sum . map (solve2 . parse)) inputLines

-- >>> map (solve1 . parse) _example
-- [18,28,68]
solve1 :: [Int] -> Int
solve1 = foldr ((+) . last) 0 . getDiffs

-- The other way around
-- >>> map (solve2 . parse) _example
-- [-3,0,5]
solve2 :: [Int] -> Int
solve2 = foldr ((-) . head) 0 . getDiffs

-- >>> getDiffs [0, 3, 6, 9, 12, 15]
-- [[0,3,6,9,12,15],[3,3,3,3,3]]
-- >>> getDiffs [1, 3, 6, 10, 15, 21]
-- [[1,3,6,10,15,21],[2,3,4,5,6],[1,1,1,1]]
-- >>> getDiffs [10, 13, 16, 21, 30, 45]
-- [[10,13,16,21,30,45],[3,3,5,9,15],[0,2,4,6],[2,2,2]]
getDiffs :: [Int] -> [[Int]]
getDiffs = takeWhile (any (/= 0)) . iterate diffs

-- >>> diffs [0, 3, 6, 9, 12, 15]
-- [3,3,3,3,3]
diffs :: [Int] -> [Int]
diffs l = zipWith (-) (tail l) l

parse :: String -> [Int]
parse = map read . words

_example :: [String]
_example =
  [ "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  ]
