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
  putStrLn $ "Result 1: " <> (show . solve01 . parse) inputLines
  putStrLn $ "Result 2: " <> (show . solve02 . parse) inputLines

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

-- Alternative fusing intervals, which is much more efficient than brute-force (same result)
-- This exploits the creation of a Semigroup instance for RangeMap to combine them into one,
-- extending the intervals to cover the whole range of possible values (0..maxBound),
-- as well as converting the input seed intervals into a RangeMap to combine.
-- Credit: https://github.com/gruhn/advent-of-code/blob/master/2023/Day05.hs
solve02' :: Almanac -> Int
solve02' almanac = (minimum . map destination . ranges) seedRanges'
  where
    seeds' = seeds almanac
    squashed =
      squashedRangeMap $
        NE.fromList $
          map
            (\f -> f almanac)
            [ seedToSoilMap,
              soilToFertilizerMap,
              fertilizerToWaterMap,
              waterToLightMap,
              lightToTemperatureMap,
              temperatureToHumidityMap,
              humidityToLocationMap
            ]
    seedRanges' = rangeMapFromSeeds seeds' <> squashed

rangeMapFromSeeds :: [Int] -> RangeMap
rangeMapFromSeeds = RangeMap . go
  where
    go :: [Int] -> [Range]
    go [] = []
    go [_] = error "odd number of seeds"
    go (src : len : rest) = Range src src len : go rest

squashedRangeMap :: NE.NonEmpty RangeMap -> RangeMap
squashedRangeMap = sconcat . fmap fillRanges

fillRanges :: RangeMap -> RangeMap
fillRanges (RangeMap rngs) =
  let fill :: Int -> [Range] -> [Range]
      fill index [] = [Range index index (maxBound - index)]
      fill index (rng : rngs') =
        let fillBefore = Range index index (source rng - index)
            newIndex = source rng + range rng
         in -- fill_before might be empty/negative range ==> filter out in second pass
            fillBefore : rng : fill newIndex rngs'
   in RangeMap $ filter ((> 0) . range) $ fill 0 $ sortOn source rngs

instance Semigroup RangeMap where
  RangeMap rangesA <> RangeMap rangesB = RangeMap $ do
    -- compute overlap for each pair for ranges
    Range destA srcA lenA <- rangesA
    Range destB srcB lenB <- rangesB

    let dest = destB + max 0 (destA - srcB)
        src = srcA + max 0 (srcB - destA)
        len = min (destA + lenA) (srcB + lenB) - max destA srcB

        overlap = Range dest src len

    guard $ range overlap > 0
    return overlap

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
