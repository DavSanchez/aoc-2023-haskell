module Day05 (day05) where

import Control.Monad (guard, void)
import Data.Char (isDigit)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Semigroup (sconcat)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S, satisfy, sepBy1, skipSpaces, string)

day05 :: IO ()
day05 = do
  inputLines <- getDataFileName "day05-input.txt" >>= readFile
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 01: " <> (show . solve01 . parse) inputLines
  putStrLn $ "Result 02: " <> (show . solve02 . parse) inputLines

-- Almanac type
data Almanac = Almanac
  { seeds :: [Int],
    seedToSoilMap :: RangeMap,
    soilToFertilizerMap :: RangeMap,
    fertilizerToWaterMap :: RangeMap,
    waterToLightMap :: RangeMap,
    lightToTemperatureMap :: RangeMap,
    temperatureToHumidityMap :: RangeMap,
    humidityToLocationMap :: RangeMap
  }
  deriving stock (Show)

data Range = Range
  { destination :: Int,
    source :: Int,
    range :: Int
  }
  deriving stock (Show)

newtype RangeMap = RangeMap
  { ranges :: [Range]
  }
  deriving stock (Show)

solve01 :: Almanac -> Int
solve01 almanac = minimum $ map (getSeedLocation almanac) (seeds almanac)

solve02 :: Almanac -> Int
solve02 almanac = foldr (min . getSeedLocation almanac) maxBound (seedRanges (seeds almanac))

getSeedLocation :: Almanac -> Int -> Int
getSeedLocation almanac =
  mapIfInRange (humidityToLocationMap almanac)
    . mapIfInRange (temperatureToHumidityMap almanac)
    . mapIfInRange (lightToTemperatureMap almanac)
    . mapIfInRange (waterToLightMap almanac)
    . mapIfInRange (fertilizerToWaterMap almanac)
    . mapIfInRange (soilToFertilizerMap almanac)
    . mapIfInRange (seedToSoilMap almanac)

generateSeedRange :: Int -> Int -> [Int]
generateSeedRange start rng = take rng [start ..]

-- >>> seedRanges [79, 14, 55, 13]
-- 27
seedRanges :: [Int] -> [Int]
seedRanges [] = []
seedRanges [_] = []
seedRanges (s1 : s2 : ss) = generateSeedRange s1 s2 <> seedRanges ss

inRange :: Int -> Range -> Bool
inRange i (Range _ src rng) = i >= src && i < src + rng

mapIfInRange :: RangeMap -> Int -> Int
mapIfInRange rngs i = if null rangeList then i else computeMapping (head rangeList) i
  where
    rangeList = filter (inRange i) (ranges rngs)
    computeMapping (Range dest src _) i' = dest + (i' - src)

-- Parser for Almanac
parse :: String -> Almanac
parse = fst . last . readP_to_S parseAlmanac

parseAlmanac :: ReadP Almanac
parseAlmanac =
  Almanac
    <$> parseSeeds
    <*> parseMap "seed-to-soil"
    <*> parseMap "soil-to-fertilizer"
    <*> parseMap "fertilizer-to-water"
    <*> parseMap "water-to-light"
    <*> parseMap "light-to-temperature"
    <*> parseMap "temperature-to-humidity"
    <*> parseMap "humidity-to-location"

parseSeeds :: ReadP [Int]
parseSeeds = do
  void $ string "seeds: "
  sepBy1 parseNumber (string " ")

parseMap :: String -> ReadP RangeMap
parseMap mapStr = do
  skipSpaces
  void $ string (mapStr <> " map:")
  skipSpaces
  RangeMap <$> sepBy1 parseRange (string "\n")

parseRange :: ReadP Range
parseRange = Range <$> parseNumber <*> parseNumber <*> parseNumber

parseNumber :: ReadP Int
parseNumber = read <$> (skipSpaces >> many1 (satisfy isDigit))

_example01 :: String
_example01 =
  "seeds: 79 14 55 13\n\
  \seed-to-soil map:\n\
  \50 98 2\n\
  \52 50 48\n\
  \\n\
  \soil-to-fertilizer map:\n\
  \0 15 37\n\
  \37 52 2\n\
  \39 0 15\n\
  \\n\
  \fertilizer-to-water map:\n\
  \49 53 8\n\
  \0 11 42\n\
  \42 0 7\n\
  \57 7 4\n\
  \\n\
  \water-to-light map:\n\
  \88 18 7\n\
  \18 25 70\n\
  \\n\
  \light-to-temperature map:\n\
  \45 77 23\n\
  \81 45 19\n\
  \68 64 13\n\
  \\n\
  \temperature-to-humidity map:\n\
  \0 69 1\n\
  \1 0 69\n\
  \\n\
  \humidity-to-location map:\n\
  \60 56 37\n\
  \56 93 4"
