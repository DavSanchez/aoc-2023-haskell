module Day07 (day07) where

import Data.Function (on)
import Data.List (group, sort, sortBy)
import Paths_aoc2023 (getDataFileName)

day07 :: IO ()
day07 = do
  inputLines <- lines <$> (getDataFileName "day07-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 01: " <> (show . winnings . parse) inputLines

-- putStrLn $ "Result 02: " <> (show . winnings2 . parse2) inputLines

data Label = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A deriving stock (Show, Eq, Ord)

type Hand = [Label]

type Bid = Int

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving stock (Show, Eq, Ord)

-- >>> winnings _example
-- 6440
winnings :: [(Hand, Bid)] -> Int
winnings = sum . map (\((_, bid), rank) -> bid * rank) . withOrder . sortHands
  where
    sortHands = sortBy (compareHands `on` fst)
    withOrder = flip zip [1 ..]

-- >>> compareHands (parseHand "33332") (parseHand "2AAAA")
-- GT
-- >>> compareHands (parseHand "77888") (parseHand "77788")
-- GT
compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 = case compare (handPoints hand1) (handPoints hand2) of
  EQ -> compare hand1 hand2
  x -> x

handPoints :: Hand -> HandType
handPoints hand = case (sort . map length . group . sort) hand of
  [5] -> FiveOfAKind
  [1, 4] -> FourOfAKind
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeOfAKind
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> OnePair
  [1, 1, 1, 1, 1] -> HighCard
  _ -> error "Invalid hand"

-- Manual parsing

parse :: [String] -> [(Hand, Bid)]
parse = map parseHandBid

parseHandBid :: String -> (Hand, Bid)
parseHandBid = handBid . words
  where
    handBid [hand, bid] = (parseHand hand, read bid)
    handBid _ = error "Invalid hand or bid"

parseHand :: String -> Hand
parseHand = map parseLabel

parseLabel :: Char -> Label
parseLabel 'A' = A
parseLabel 'K' = K
parseLabel 'Q' = Q
parseLabel 'J' = J
parseLabel 'T' = T
parseLabel '9' = N9
parseLabel '8' = N8
parseLabel '7' = N7
parseLabel '6' = N6
parseLabel '5' = N5
parseLabel '4' = N4
parseLabel '3' = N3
parseLabel '2' = N2
parseLabel _ = error "Invalid label"

_example :: [(Hand, Bid)]
_example =
  [ (parseHand "32T3K", 765),
    (parseHand "T55J5", 684),
    (parseHand "KK677", 28),
    (parseHand "KTJJT", 220),
    (parseHand "QQQJA", 483)
  ]
