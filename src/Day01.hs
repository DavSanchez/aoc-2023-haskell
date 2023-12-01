module Day01 (day01) where

import Data.Char (isNumber)
import Data.List qualified as L
import Paths_aoc2023 (getDataFileName)

day01 :: IO ()
day01 = do
  inputLines <- lines <$> (getDataFileName "day01-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> show (process calibrationValue01 inputLines)
  putStrLn $ "Result 2: " <> show (process calibrationValue02 inputLines)

process :: (String -> Int) -> [String] -> Int
process f = sum . map f

calibrationValue01 :: String -> Int
calibrationValue01 = read . buildNumberText . filter isNumber

calibrationValue02 :: String -> Int
calibrationValue02 = read . buildNumberText . mconcat . map fromStringPrefix . L.tails

buildNumberText :: String -> String
buildNumberText [] = "00"
buildNumberText s = head s : [last s]

fromStringPrefix :: String -> String
fromStringPrefix (x : _) | isNumber x = L.singleton x
fromStringPrefix s | "zero" `L.isPrefixOf` s = "0"
fromStringPrefix s | "one" `L.isPrefixOf` s = "1"
fromStringPrefix s | "two" `L.isPrefixOf` s = "2"
fromStringPrefix s | "three" `L.isPrefixOf` s = "3"
fromStringPrefix s | "four" `L.isPrefixOf` s = "4"
fromStringPrefix s | "five" `L.isPrefixOf` s = "5"
fromStringPrefix s | "six" `L.isPrefixOf` s = "6"
fromStringPrefix s | "seven" `L.isPrefixOf` s = "7"
fromStringPrefix s | "eight" `L.isPrefixOf` s = "8"
fromStringPrefix s | "nine" `L.isPrefixOf` s = "9"
fromStringPrefix _ = mempty
