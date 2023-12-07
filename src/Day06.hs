module Day06 (day06) where

import Data.Char (isDigit)
import Paths_aoc2023 (getDataFileName)

day06 :: IO ()
day06 = do
  inputLines <- lines <$> (getDataFileName "day06-input.txt" >>= readFile)
  putStrLn $ "Result 01: " <> (show . solve01 . parse) inputLines
  putStrLn $ "Result 02: " <> (show . solve02 . parse2) inputLines

data Race = Race
  { durationMs :: Int,
    recordDistance :: Int
  }
  deriving stock (Show)

solve01 :: [Race] -> Int
solve01 = product . map computeWinningHolds

solve02 :: Race -> Int
solve02 = computeWinningHolds

-- >>> computeWinningHolds (Race 7 9)
-- 4
-- >>> computeWinningHolds (Race 15 40)
-- 8
-- >>> computeWinningHolds (Race 30 200)
-- 9
computeWinningHolds :: Race -> Int
computeWinningHolds (Race duration distance)
  | odd duration = 2 * length winList
  | otherwise = 2 * length winList - 1
  where
    optimal = duration `div` 2
    optimalSemiList = [optimal, optimal - 1 ..]
    winList = takeWhile (> distance) (raceEquation duration <$> optimalSemiList)

-- >>> raceEquation 7 0
-- 0
-- >>> raceEquation 7 7
-- 0
-- >>> raceEquation 7 2
-- 10
-- >>> raceEquation 7 3
-- 12
-- >>> raceEquation 7 4
-- 12
-- >>> raceEquation 7 5
-- 10
raceEquation :: Int -> Int -> Int
raceEquation availableTime hold = hold * (availableTime - hold)

-- Extreme for y = x * (time - x)
-- >>> maxDistance 7
-- 12
-- >>> maxDistance 15
-- 56
_maxDistance :: Int -> Int
_maxDistance availableTime = (availableTime ^ (2 :: Int)) `div` 4

parse :: [String] -> [Race]
parse [t, d] = zipWith Race (nums t) (nums d)
  where
    noLabel = dropWhile (not . isDigit)
    nums = map read . words . noLabel
parse _ = error "Invalid input"

parse2 :: [String] -> Race
parse2 [t, d] = Race (num t) (num d)
  where
    num = read . filter isDigit
parse2 _ = error "Invalid input"
