module Main (main) where

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)
import Day04 (day04)
import Day05 (day05)
import Day06 (day06)
import Day07 (day07)
import Day08 (day08)
import Day09 (day09)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1" : _ -> day01
    "2" : _ -> day02
    "3" : _ -> day03
    "4" : _ -> day04
    "5" : _ -> day05
    "6" : _ -> day06
    "7" : _ -> day07
    "8" : _ -> day08
    "9" : _ -> day09
    _ -> error "None or invalid day number provided."
