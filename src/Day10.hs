module Day10 (day10) where

import Data.Map qualified as M
import Paths_aoc2023 (getDataFileName)

day10 :: IO ()
day10 = do
  inputLines <- getDataFileName "day10-input.txt" >>= readFile
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Solution 1: " <> (show . solve1 . grid) inputLines
  putStrLn $ "Solution 2: " <> (show . solve2 . grid) inputLines

-- putStrLn $ "Solution 2': " <> (show . solve2') inputLines

type Coords = (Int, Int)

type Grid = M.Map Coords Char

-- data PipeDirection = NorthSouth | NorthEast | NorthWest | EastWest | SouthEast | SouthWest deriving stock (Show, Eq)

-- data GridElement = Invalid | Ground | StartingPosition | Pipe PipeDirection deriving stock (Show, Eq)

-- >>> solve1 (grid _simpleExample)
-- 4
-- >>> solve1 (grid _complexExample)
-- 8
solve1 :: Grid -> Int
solve1 grd = (`div` 2) $ length $ traverseGrid grd (startingCoords grd) (startingCoords grd)

-- >>> solve2 (grid _simpleExample)
-- 1
-- >>> solve2 (grid _complexExample)
-- 1
-- >>> solve2 (grid _part2Example)
-- 4
-- >>> solve2 (grid _part2Example2)
-- 8
-- >>> solve2 (grid _part2Example3)
-- 10
solve2 :: Grid -> Int
solve2 grd = picksTheoremForInteriorPoints areaTimes2 (length boundaryPoints)
  where
    boundaryPoints = traverseGrid grd (startingCoords grd) (startingCoords grd)
    areaTimes2 = abs $ shoelaceFormula (boundaryPoints <> [head boundaryPoints])

-- Pick's Theorem: A = i + (b / 2) - 1 --> https://en.wikipedia.org/wiki/Pick's_theorem
-- For 2A = 2i + b - 2
-- A = area of the polygon
-- i = number of interior points
-- b = number of boundary points
picksTheoremForInteriorPoints :: Int -> Int -> Int
picksTheoremForInteriorPoints areaTimes2 boundaryPoints = (areaTimes2 - boundaryPoints + 2) `div` 2

-- Shoelace formula: https://en.wikipedia.org/wiki/Shoelace_formula
-- This is for getting double the area of a polygon, 2A to use in Pick's Theorem above.
-- 2A is the determinant of all point pairs of the polygon.
shoelaceFormula :: [Coords] -> Int
shoelaceFormula ((x1, y1) : (x2, y2) : rest) = (x1 * y2) - (y1 * x2) + shoelaceFormula ((x2, y2) : rest)
shoelaceFormula [_] = 0
shoelaceFormula [] = 0

canConnectFromNorth :: Char -> Char -> Bool
canConnectFromNorth a b = a `elem` ['S', '|', 'L', 'J'] && b `elem` ['|', 'F', '7']

canConnectFromSouth :: Char -> Char -> Bool
canConnectFromSouth a b = a `elem` ['S', '|', 'F', '7'] && b `elem` ['|', 'L', 'J']

canConnectFromEast :: Char -> Char -> Bool
canConnectFromEast a b = a `elem` ['S', '-', 'L', 'F'] && b `elem` ['-', 'J', '7']

canConnectFromWest :: Char -> Char -> Bool
canConnectFromWest a b = a `elem` ['S', '-', 'J', '7'] && b `elem` ['-', 'L', 'F']

-- >>> traverseGrid (grid _simpleExample) (1,1)
traverseGrid :: Grid -> Coords -> Coords -> [Coords]
traverseGrid grd c1 crds@(x, y)
  | canConnectFromNorth (getPipe crds) (getPipe north) && c1 /= north = crds : traverseGrid grd crds north -- Went North
  | canConnectFromEast (getPipe crds) (getPipe east) && c1 /= east = crds : traverseGrid grd crds east -- Went East
  | canConnectFromSouth (getPipe crds) (getPipe south) && c1 /= south = crds : traverseGrid grd crds south -- Went South
  | canConnectFromWest (getPipe crds) (getPipe west) && c1 /= west = crds : traverseGrid grd crds west -- Went West
  | otherwise = [crds]
  where
    north = (x, y - 1)
    east = (x + 1, y)
    south = (x, y + 1)
    west = (x - 1, y)
    getPipe crds' = M.findWithDefault 'x' crds' grd

-- >>> grid _simpleExample
-- fromList [((0,0),'.'),((0,1),'.'),((0,2),'.'),((0,3),'.'),((0,4),'.'),((1,0),'.'),((1,1),'S'),((1,2),'|'),((1,3),'L'),((1,4),'.'),((2,0),'.'),((2,1),'-'),((2,2),'.'),((2,3),'-'),((2,4),'.'),((3,0),'.'),((3,1),'7'),((3,2),'|'),((3,3),'J'),((3,4),'.'),((4,0),'.'),((4,1),'.'),((4,2),'.'),((4,3),'.'),((4,4),'.')]
grid :: String -> Grid
grid = M.fromList . concat . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0 ..]) [0 ..] . lines

-- >>> startingCoords $ grid _simpleExample
-- (1,1)
-- >>> startingCoords $ grid _complexExample
-- (0,2)
startingCoords :: Grid -> Coords
startingCoords = fst . head . filter ((== 'S') . snd) . M.toList

_simpleExample :: String
_simpleExample =
  unlines
    [ ".....",
      ".S-7.",
      ".|.|.",
      ".L-J.",
      "....."
    ]

_complexExample :: String
_complexExample =
  unlines
    [ "..F7.",
      ".FJ|.",
      "SJ.L7",
      "|F--J",
      "LJ..."
    ]

_part2Example :: String
_part2Example =
  unlines
    [ "...........",
      ".S-------7.",
      ".|F-----7|.",
      ".||.....||.",
      ".||.....||.",
      ".|L-7.F-J|.",
      ".|..|.|..|.",
      ".L--J.L--J.",
      "..........."
    ]

_part2Example2 :: String
_part2Example2 =
  unlines
    [ ".F----7F7F7F7F-7....",
      ".|F--7||||||||FJ....",
      ".||.FJ||||||||L7....",
      "FJL7L7LJLJ||LJ.L-7..",
      "L--J.L7...LJS7F-7L7.",
      "....F-J..F7FJ|L7L7L7",
      "....L7.F7||L7|.L7L7|",
      ".....|FJLJ|FJ|F7|.LJ",
      "....FJL-7.||.||||...",
      "....L---J.LJ.LJLJ..."
    ]

_part2Example3 :: String
_part2Example3 =
  unlines
    [ "FF7FSF7F7F7F7F7F---7",
      "L|LJ||||||||||||F--J",
      "FL-7LJLJ||||||LJL-77",
      "F--JF--7||LJLJ7F7FJ-",
      "L---JF-JLJ.||-FJLJJ7",
      "|F|F-JF---7F7-L7L|7|",
      "|FFJF7L7F-JF7|JL---7",
      "7-L-JL7||F7|L7F-7F7|",
      "L.L7LFJ|||||FJL7||LJ",
      "L7JLJL-JLJLJL--JLJ.L"
    ]
