{-# LANGUAGE DerivingStrategies #-}

module Day03 (day03) where

import Data.Char (isDigit)
import Data.List (groupBy, nub)
import Paths_aoc2023 (getDataFileName)

day03 :: IO ()
day03 = do
  inputLines <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> (show . sum . process01) inputLines
  putStrLn $ "Result 2: " <> (show . sum . process02) inputLines

type Matrix = [String]

type Coords = (Int, Int)

type CharWithCoords = (Char, Coords)

data NumWithGears = NumWithGears
  { num :: Int,
    gears :: [Coords]
  }
  deriving stock (Show)

_testSchematic :: Matrix
_testSchematic =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

process01 :: Matrix -> [Int]
process01 mat = (map getNumber . filter (hasAdjacentSymbol mat) . groupNumbers . withCoords) mat

process02 :: Matrix -> [Int]
process02 mat = map product numberPairs
  where
    numsWithGears = (map (buildNumWithGears mat) . filter (hasAdjacentGear mat) . groupNumbers . withCoords) mat
    allGears = getAllGears numsWithGears
    numberGroups = map (`numbersConnectedToGear` numsWithGears) allGears
    numberPairs = filter ((== 2) . length) numberGroups

-- >>> numbersConnectedToGear (4, 2) [NumWithGears {num = 467, gears = [(4, 2)]}]
-- [467]
-- >>> numbersConnectedToGear (4, 2) [NumWithGears {num = 467, gears = [(4, 2)]}, NumWithGears {num = 114, gears = [(6, 1), (4, 2), (8, 1)]}]
-- [467,114]
numbersConnectedToGear :: Coords -> [NumWithGears] -> [Int]
numbersConnectedToGear coords = map num . filter (elem coords . gears)

-- >>> getAllGears $ [NumWithGears {num = 467, gears = [(4, 2)]}]
-- [(4,2)]
-- >>> getAllGears $ [NumWithGears {num = 467, gears = [(4, 2)]}, NumWithGears {num = 114, gears = [(6, 1), (4, 2), (8, 1)]}]
-- [(4,2),(6,1),(8,1)]
getAllGears :: [NumWithGears] -> [Coords]
getAllGears = nub . concatMap gears

-- >>> getNumber [('4',(1,1))]
-- 4
-- >>> getNumber [('4',(1,1)),('6',(2,1)),('7',(3,1))]
-- 467
-- >>> getNumber []
-- 0
getNumber :: [CharWithCoords] -> Int
getNumber cwc = if null numStr then 0 else read numStr
  where
    numStr = map fst cwc

-- >>> buildNumWithGears _testSchematic [('4',(1,1)),('6',(2,1)),('7',(3,1))]
-- NumWithGears {num = 467, gears = [(4,2)]}
buildNumWithGears :: Matrix -> [CharWithCoords] -> NumWithGears
buildNumWithGears m cwc = NumWithGears (getNumber cwc) getGears
  where
    searchMatrix = withPadding m
    adjacentGears coords =
      filter
        (\(x, y) -> searchMatrix !! y !! x == '*')
        [ (fst coords, snd coords - 1), -- north
          (fst coords, snd coords + 1), -- south
          (fst coords + 1, snd coords), -- east
          (fst coords - 1, snd coords), -- west
          (fst coords + 1, snd coords - 1), -- northeast
          (fst coords - 1, snd coords - 1), -- northwest
          (fst coords + 1, snd coords + 1), -- southeast
          (fst coords - 1, snd coords + 1) -- southwest
        ]
    getGears = nub $ concatMap (adjacentGears . snd) cwc

-- >>> hasAdjacentGear _testSchematic [('4',(1,1)),('6',(2,1)),('7',(3,1))]
-- True
-- >>> hasAdjacentGear _testSchematic [('1',(6,1)),('1',(7,1)),('4',(8,1))]
-- False
hasAdjacentGear :: Matrix -> [CharWithCoords] -> Bool
hasAdjacentGear m = any (adjacentGear . snd)
  where
    searchMatrix = withPadding m
    north (x, y) = searchMatrix !! (y - 1) !! x
    south (x, y) = searchMatrix !! (y + 1) !! x
    east (x, y) = searchMatrix !! y !! (x + 1)
    west (x, y) = searchMatrix !! y !! (x - 1)
    northeast (x, y) = searchMatrix !! (y - 1) !! (x + 1)
    northwest (x, y) = searchMatrix !! (y - 1) !! (x - 1)
    southeast (x, y) = searchMatrix !! (y + 1) !! (x + 1)
    southwest (x, y) = searchMatrix !! (y + 1) !! (x - 1)
    isGear c = c == '*'
    adjacentGear coords =
      any
        isGear
        [ north coords,
          south coords,
          east coords,
          west coords,
          northeast coords,
          northwest coords,
          southeast coords,
          southwest coords
        ]

-- >>> hasAdjacentSymbol _testSchematic [('4',(1,1)),('6',(2,1)),('7',(3,1))]
-- True
-- >>> hasAdjacentSymbol _testSchematic [('1',(6,1)),('1',(7,1)),('4',(8,1))]
-- False
hasAdjacentSymbol :: Matrix -> [CharWithCoords] -> Bool
hasAdjacentSymbol m = any (adjacentSymbol . snd)
  where
    searchMatrix = withPadding m
    north (x, y) = searchMatrix !! (y - 1) !! x
    south (x, y) = searchMatrix !! (y + 1) !! x
    east (x, y) = searchMatrix !! y !! (x + 1)
    west (x, y) = searchMatrix !! y !! (x - 1)
    northeast (x, y) = searchMatrix !! (y - 1) !! (x + 1)
    northwest (x, y) = searchMatrix !! (y - 1) !! (x - 1)
    southeast (x, y) = searchMatrix !! (y + 1) !! (x + 1)
    southwest (x, y) = searchMatrix !! (y + 1) !! (x - 1)
    isSymbol c = not $ isDigit c || c == '.'
    adjacentSymbol coords =
      any
        isSymbol
        [ north coords,
          south coords,
          east coords,
          west coords,
          northeast coords,
          northwest coords,
          southeast coords,
          southwest coords
        ]

-- >>> groupNumbers (withCoords _testSchematic)
-- [[('4',(1,1)),('6',(2,1)),('7',(3,1))],[('1',(6,1)),('1',(7,1)),('4',(8,1))],[('3',(3,3)),('5',(4,3))],[('6',(7,3)),('3',(8,3)),('3',(9,3))],[('6',(1,5)),('1',(2,5)),('7',(3,5))],[('5',(8,6)),('8',(9,6))],[('5',(3,7)),('9',(4,7)),('2',(5,7))],[('7',(7,8)),('5',(8,8)),('5',(9,8))],[('6',(2,10)),('6',(3,10)),('4',(4,10))],[('5',(6,10)),('9',(7,10)),('8',(8,10))]]
groupNumbers :: [CharWithCoords] -> [[CharWithCoords]]
groupNumbers = filter (all (isDigit . fst)) . groupBy (\a b -> isDigit (fst a) && isDigit (fst b))

-- >>> withCoords _testSchematic
-- [('4',(1,1)),('6',(2,1)),('7',(3,1)),('.',(4,1)),('.',(5,1)),('1',(6,1)),('1',(7,1)),('4',(8,1)),('.',(9,1)),('.',(10,1)),('.',(1,2)),('.',(2,2)),('.',(3,2)),('*',(4,2)),('.',(5,2)),('.',(6,2)),('.',(7,2)),('.',(8,2)),('.',(9,2)),('.',(10,2)),('.',(1,3)),('.',(2,3)),('3',(3,3)),('5',(4,3)),('.',(5,3)),('.',(6,3)),('6',(7,3)),('3',(8,3)),('3',(9,3)),('.',(10,3)),('.',(1,4)),('.',(2,4)),('.',(3,4)),('.',(4,4)),('.',(5,4)),('.',(6,4)),('#',(7,4)),('.',(8,4)),('.',(9,4)),('.',(10,4)),('6',(1,5)),('1',(2,5)),('7',(3,5)),('*',(4,5)),('.',(5,5)),('.',(6,5)),('.',(7,5)),('.',(8,5)),('.',(9,5)),('.',(10,5)),('.',(1,6)),('.',(2,6)),('.',(3,6)),('.',(4,6)),('.',(5,6)),('+',(6,6)),('.',(7,6)),('5',(8,6)),('8',(9,6)),('.',(10,6)),('.',(1,7)),('.',(2,7)),('5',(3,7)),('9',(4,7)),('2',(5,7)),('.',(6,7)),('.',(7,7)),('.',(8,7)),('.',(9,7)),('.',(10,7)),('.',(1,8)),('.',(2,8)),('.',(3,8)),('.',(4,8)),('.',(5,8)),('.',(6,8)),('7',(7,8)),('5',(8,8)),('5',(9,8)),('.',(10,8)),('.',(1,9)),('.',(2,9)),('.',(3,9)),('$',(4,9)),('.',(5,9)),('*',(6,9)),('.',(7,9)),('.',(8,9)),('.',(9,9)),('.',(10,9)),('.',(1,10)),('6',(2,10)),('6',(3,10)),('4',(4,10)),('.',(5,10)),('5',(6,10)),('9',(7,10)),('8',(8,10)),('.',(9,10)),('.',(10,10))]
withCoords :: Matrix -> [CharWithCoords]
withCoords = concat . zipWith (\y row -> zipWith (\x c -> (c, (x, y))) [1 ..] row) [1 ..]

-- >>> withPadding _testSchematic
-- ["............",".467..114...","....*.......","...35..633..",".......#....",".617*.......","......+.58..","...592......",".......755..","....$.*.....","..664.598...","............"]
withPadding :: Matrix -> Matrix
withPadding mat =
  let len = (length . head) mat
      topBottomPadding = replicate (len + 2) '.'
   in topBottomPadding : map (\row -> '.' : row ++ ".") mat ++ [topBottomPadding]
