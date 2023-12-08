module Day07 (day07) where

import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (group, maximumBy, sort, sortBy)
import Paths_aoc2023 (getDataFileName)

day07 :: IO ()
day07 = do
  inputLines <- lines <$> (getDataFileName "day07-input.txt" >>= readFile)
  -- putStrLn "This is what I read from input:"
  -- putStrLn $ unlines inputLines
  putStrLn $ "Result 1: " <> (show . winnings compareHands . parse) inputLines
  putStrLn $ "Result 2: " <> (show . winnings compareHands2 . map (first newJokers) . parse) inputLines

data Label = Joker | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A deriving stock (Show, Eq, Ord, Enum, Bounded)

type Hand = [Label]

type Bid = Int

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving stock (Show, Eq, Ord)

-- >>> winnings compareHands _example
-- 6440
-- >>> winnings compareHands2 (map (first newJokers) _example)
-- 5905
winnings :: (Hand -> Hand -> Ordering) -> [(Hand, Bid)] -> Int
winnings ordFn = sum . map (\((_, bid), rank) -> bid * rank) . withOrder . sortHands
  where
    sortHands = sortBy (ordFn `on` fst)
    withOrder = flip zip [1 ..]

-- >>> map handValue2 $ map fst _example
-- Variable not in scope: handValue2 :: Hand -> b_alRaF[sk:1]
newJokers :: Hand -> Hand
newJokers = map (\c -> if c == J then Joker else c)

-- >>> map replaceJokers $ (map (newJokers . fst) _example) <> [[Joker,Joker,Joker,Joker,Joker]]
-- [[N3,N2,T,N3,K],[T,N5,N5,N5,N5],[K,K,N6,N7,N7],[K,T,T,T,T],[Q,Q,Q,Q,A],[A,A,A,A,A]]
replaceJokers :: Hand -> Hand
replaceJokers hand = maximumBy (compare `on` handPoints) $ do
  replacementCard <- filter (\l -> l /= J && l /= Joker) [minBound .. maxBound]
  return $ map (replaceBy replacementCard) hand
  where
    replaceBy rep c = if c == Joker then rep else c

-- >>> compareHands (parseHand "33332") (parseHand "2AAAA")
-- GT
-- >>> compareHands (parseHand "T55J5") (parseHand "KTJJT")
-- GT
compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 = case compare (handPoints hand1) (handPoints hand2) of
  EQ -> compare hand1 hand2
  x -> x

-- >>> compareHands2 (parseHand "33332") (parseHand "2AAAA")
-- GT
-- >>> compareHands2 (newJokers $ parseHand "T55J5") (newJokers $ parseHand "KTJJT")
-- LT
compareHands2 :: Hand -> Hand -> Ordering
compareHands2 hand1 hand2 = case compare ((handPoints . replaceJokers) hand1) ((handPoints . replaceJokers) hand2) of
  EQ -> compare hand1 hand2
  x -> x

-- >>> sort $ map handPoints $ map (replaceJokers . fst) _example
-- [OnePair,TwoPair,FourOfAKind,FourOfAKind,FourOfAKind]
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
  map
    (first parseHand)
    [ ("32T3K", 765),
      ("T55J5", 684),
      ("KK677", 28),
      ("KTJJT", 220),
      ("QQQJA", 483) -- , ("JJJJJ", 0)
    ]
