module Day08 (day08) where

import Data.List (isSuffixOf)
import Data.Map.Strict qualified as M
import Paths_aoc2023 (getDataFileName)

day08 :: IO ()
day08 = do
  inputLines <- getDataFileName "day08-input.txt" >>= readFile
  -- putStrLn "This is what I read from input:"
  putStrLn $ "Result 1: " <> (show . length . solve1 . parse) inputLines
  putStrLn $ "Result 2: " <> (show . foldl1 lcm . map length . solve2 . parse) inputLines

type Sequence = String

type Node = String

type Nodes = M.Map Node NextNode

type NextNode = (Node, Node)

-- >>> solve2 $ parse _example3
-- [["11A","11B"],["22A","22B","22C"]]
-- >>> foldl1 lcm $ map length $ solve2 $ parse _example3
-- 6
solve2 :: (Sequence, Nodes) -> [[Node]]
solve2 (sqn, nodes) = map buildList startNodes
  where
    startNodes = filter ("A" `isSuffixOf`) $ M.keys nodes
    buildList l = takeWhile (not . ("Z" `isSuffixOf`)) $ scanl (nextNode nodes) l (cycle sqn)

-- >>> solve1 $ parse _example
-- ["AAA","CCC"]
-- >>> solve1 $ parse _example2
-- ["AAA","BBB","AAA","BBB","AAA","BBB"]
solve1 :: (Sequence, Nodes) -> [Node]
solve1 (sqn, nodes) = takeWhile (/= "ZZZ") $ scanl (nextNode nodes) "AAA" (cycle sqn)

-- >>> nextNode (snd $ parse _example) "AAA" 'R'
-- Just "CCC"
-- >>> nextNode (snd $ parse _example) "CCC" 'L'
-- Just "ZZZ"
-- >>> nextNode (snd $ parse _example) "ZZZ" 'L'
-- Nothing
nextNode :: Nodes -> Node -> Char -> Node
nextNode nodes n 'L' = fst $ nodes M.! n
nextNode nodes n 'R' = snd $ nodes M.! n
nextNode _ _ _ = error "nextNode: invalid input"

-- >>> parse _example
-- ("RL",fromList [("AAA",("BBB","CCC")),("BBB",("DDD","EEE")),("CCC",("ZZZ","GGG")),("DDD",("DDD","DDD")),("EEE",("EEE","EEE")),("GGG",("GGG","GGG")),("ZZZ",("ZZZ","ZZZ"))])
parse :: String -> (Sequence, Nodes)
parse input = (getSequence, nodes)
  where
    eachLine = filter (not . null) $ lines input
    getSequence = head eachLine
    nodes = M.fromList $ map (parseLine . preProcess) $ tail eachLine
    parseLine [s, l, r] = (s, (l, r))
    parseLine _ = error "parseLine: invalid input"
    preProcess = words . filter (`notElem` ['=', ',', '(', ')'])

_example :: String
_example =
  unlines
    [ "RL",
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)"
    ]

_example2 :: String
_example2 =
  unlines
    [ "LLR",
      "AAA = (BBB, BBB)",
      "BBB = (AAA, ZZZ)",
      "ZZZ = (ZZZ, ZZZ)"
    ]

_example3 :: String
_example3 =
  unlines
    [ "LR",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)"
    ]
