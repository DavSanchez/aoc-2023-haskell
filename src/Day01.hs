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
calibrationValue02 = read . buildNumberText . mconcat . map fromStringDigit . L.tails

buildNumberText :: String -> String
buildNumberText [] = "0"
buildNumberText [c] = replicate 2 c
buildNumberText s = head s : [last s]

fromStringDigit :: String -> String
fromStringDigit [] = mempty
fromStringDigit s@(x : _)
  | "zero" `L.isPrefixOf` s = "0"
  | "one" `L.isPrefixOf` s = "1"
  | "two" `L.isPrefixOf` s = "2"
  | "three" `L.isPrefixOf` s = "3"
  | "four" `L.isPrefixOf` s = "4"
  | "five" `L.isPrefixOf` s = "5"
  | "six" `L.isPrefixOf` s = "6"
  | "seven" `L.isPrefixOf` s = "7"
  | "eight" `L.isPrefixOf` s = "8"
  | "nine" `L.isPrefixOf` s = "9"
  | isNumber x = [x]
  | otherwise = mempty
