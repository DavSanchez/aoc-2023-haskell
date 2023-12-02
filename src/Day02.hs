{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}

module Day02 (day02) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isDigit)
import Paths_aoc2023 (getDataFileName)
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S, satisfy, sepBy1, skipSpaces, string)

day02 :: IO ()
day02 = do
  inputLines <- lines <$> (getDataFileName "day02-input.txt" >>= readFile)
  putStrLn $ "Result 1: " <> (show . sum . process01) inputLines
  putStrLn $ "Result 2: " <> (show . sum . process02) inputLines
  where
    process01 = validGameIDs . map fromString
    process02 = map (cubeSetPower . minimumSet . fromString)

-- | A game has an ID and consists on a list of cube sets.
data Game = Game
  { -- | The game ID
    _id :: Int,
    -- | The list of cube sets
    sets :: [CubesSet]
  }
  deriving stock (Show)

-- | Parses a game from a string. See the parser combinators at the bottom of the file.
fromString :: String -> Game
fromString = fst . last . readP_to_S parseGame

-- | Gets the valid game IDs from a list of games.
validGameIDs :: [Game] -> [Int]
validGameIDs = map _id . filter isGameValid

-- | Check if a given game is valid, i.e. all its cube sets are valid.
isGameValid :: Game -> Bool
isGameValid game = all isSetValid $ sets game

-- | A set of cubes has a number of blue, green and red cubes.
data CubesSet = CubeSet
  { -- | The number of blue cubes
    blue :: Int,
    -- | The number of green cubes
    green :: Int,
    -- | The number of red cubes
    red :: Int
  }
  deriving stock (Show)

-- | The set of cubes is a semigroup, where the operation is the sum of the cubes of the same color.
instance Semigroup CubesSet where
  (<>) :: CubesSet -> CubesSet -> CubesSet
  cs1 <> cs2 =
    CubeSet
      { blue = blue cs1 + blue cs2,
        green = green cs1 + green cs2,
        red = red cs1 + red cs2
      }

-- | The set of cubes is a monoid, where the identity is no cubes of any color.
instance Monoid CubesSet where
  mempty :: CubesSet
  mempty =
    CubeSet
      { blue = 0,
        green = 0,
        red = 0
      }

-- | Check if a given cube set is valid, i.e. it does not contain more cubes than the bag.
isSetValid :: CubesSet -> Bool
isSetValid cs = blue cs <= blue bag && green cs <= green bag && red cs <= red bag

-- | Gets the minimum set of cubes that could have have generated the game.
minimumSet :: Game -> CubesSet
minimumSet game =
  CubeSet
    { blue = maximum blues,
      green = maximum greens,
      red = maximum reds
    }
  where
    sets' = sets game
    blues = map blue sets'
    greens = map green sets'
    reds = map red sets'

-- | Gets the power of a set of cubes, which is the product of the number of cubes of each color.
cubeSetPower :: CubesSet -> Int
cubeSetPower cs = blue cs * green cs * red cs

-- | The bag of cubes has 14 blue, 13 green and 12 red cubes.
bag :: CubesSet
bag =
  CubeSet
    { blue = 14,
      green = 13,
      red = 12
    }

-- | A group of cubes of the same color. Can be either blue, green or red.
data Cubes = BlueCubes Int | GreenCubes Int | RedCubes Int

-- | Adds a group of cubes to a set of cubes.
addCubes :: CubesSet -> Cubes -> CubesSet
addCubes cs (BlueCubes b) = cs {blue = blue cs + b}
addCubes cs (GreenCubes g) = cs {green = green cs + g}
addCubes cs (RedCubes r) = cs {red = red cs + r}

-- Parser combinators to get a game from an input line

parseGame :: ReadP Game
parseGame = do
  _id <- gameID
  void skipSpaces
  sets' <- sepBy1 cubeSet (string ";")
  pure $ Game _id sets'

gameID :: ReadP Int
gameID = do
  void $ string "Game "
  _id <- many1 $ satisfy isDigit
  void $ string ": "
  pure $ read _id

cubeSet :: ReadP CubesSet
cubeSet = do
  cubes <- sepBy1 ((BlueCubes <$> parseBlue) <|> (GreenCubes <$> parseGreen) <|> (RedCubes <$> parseRed)) (string ",")
  pure $ foldr (flip addCubes) mempty cubes

parseBlue :: ReadP Int
parseBlue = do
  void skipSpaces
  cubes <- many1 $ satisfy isDigit
  void $ string " blue"
  pure $ read cubes

parseGreen :: ReadP Int
parseGreen = do
  void skipSpaces
  cubes <- many1 $ satisfy isDigit
  void $ string " green"
  pure $ read cubes

parseRed :: ReadP Int
parseRed = do
  void skipSpaces
  cubes <- many1 $ satisfy isDigit
  void $ string " red"
  pure $ read cubes
