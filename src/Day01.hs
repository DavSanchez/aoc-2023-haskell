module Day01 (day01) where

import Data.Char (isNumber)
import Data.List (isPrefixOf, singleton, tails)
import Paths_aoc2023 (getDataFileName)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> show (process calibrationValue01 inputLines)
  putStrLn $ "Result 2: " <> show (process calibrationValue02 inputLines)

-- | Applies a function to each element of a list to get the result integer, and sums the results.
process :: (String -> Int) -> [String] -> Int
process f = sum . map f

-- |  Filters out the non-numeric characters of a string, takes the first and last digit, and converts it to an integer.
-- >>> calibrationValue01 "123"
-- 13
-- >>> calibrationValue01 "1234"
-- 14
calibrationValue01 :: String -> Int
calibrationValue01 = read . buildNumberText . filter isNumber

-- | Gets all the suffixes of the string, gets the numeric prefix of each suffix,
-- | filter out any empty results and generates a double-digit integer from the first and last digits.
-- >>> calibrationValue02 "one123"
-- 13
-- >>> calibrationValue02 "one123four"
-- 14
calibrationValue02 :: String -> Int
calibrationValue02 = read . buildNumberText . mconcat . map fromStringPrefix . tails

-- | Builds a double-digit number from a string of digits, using the first and the last digit.
-- >>> buildNumberText "123"
-- "13"
-- >>> buildNumberText ""
-- "00"
buildNumberText :: String -> String
buildNumberText [] = "00"
buildNumberText s = head s : [last s]

-- | Extracts the first number from a string, taking into account english textual numbers.
-- >>> fromStringPrefix "one123"
-- "1"
-- >>> fromStringPrefix "zerone123four"
-- "0"
fromStringPrefix :: String -> String
fromStringPrefix (x : _) | isNumber x = singleton x
fromStringPrefix s | "zero" `isPrefixOf` s = "0"
fromStringPrefix s | "one" `isPrefixOf` s = "1"
fromStringPrefix s | "two" `isPrefixOf` s = "2"
fromStringPrefix s | "three" `isPrefixOf` s = "3"
fromStringPrefix s | "four" `isPrefixOf` s = "4"
fromStringPrefix s | "five" `isPrefixOf` s = "5"
fromStringPrefix s | "six" `isPrefixOf` s = "6"
fromStringPrefix s | "seven" `isPrefixOf` s = "7"
fromStringPrefix s | "eight" `isPrefixOf` s = "8"
fromStringPrefix s | "nine" `isPrefixOf` s = "9"
fromStringPrefix _ = mempty
