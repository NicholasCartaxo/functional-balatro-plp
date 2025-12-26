module PokerHands
  ( PokerHand(..)
  , getPokerHandAndCards
  ) where
import Cards
import Data.List (sort,find)
import Data.Maybe (isJust,fromJust)

data PokerHand = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
  deriving (Eq, Show, Ord, Enum, Bounded)

getStraight :: [Card] -> [Card]
getStraight xs
  |length xs /= 5 = []
  |otherwise = if straightLoop (sort xs) then xs else []
  where
    straightLoop [] = True
    straightLoop [_] = True
    straightLoop [Card R5 _, Card RA _] = True
    straightLoop (Card x _:Card y __:l) =
      (y == succ x) && straightLoop (Card y __:l)

getFlush :: [Card] -> [Card]
getFlush xs
  |length xs /= 5 = []
  |otherwise = if flushLoop xs then xs else []
  where
    flushLoop [] = True
    flushLoop [_] = True
    flushLoop (Card _ x:Card __ y:l) =
      (x == y) && flushLoop (Card __ y:l)

countOccurrences :: [Card] -> [(Rank,Int)]
countOccurrences xs = [(i,j) | i <- [minBound..maxBound], j <- [length (filter (\(Card x _)-> x==i) xs)], j /= 0]

getNRepeats :: Int -> [Card] -> [Card]
getNRepeats n xs = filter isFromRank xs
  where
    occurrences = countOccurrences xs
    isFromRank (Card rank _) = Just rank == fmap fst (find (\(_,times) -> times == n) occurrences)

getFour :: [Card] -> [Card]
getFour = getNRepeats 4

getFullHouse :: [Card] -> [Card]
getFullHouse xs = filter isInRanks xs
  where
    occurrences = countOccurrences xs
    rankThree = fmap fst (find (\(_,times) -> times == 3) occurrences)
    rankTwo = fmap fst (find (\(_,times) -> times == 2) occurrences)
    isInRanks (Card rank _) = isJust rankThree && isJust rankTwo && elem (Just rank) [rankThree, rankTwo]

getThree :: [Card] -> [Card]
getThree = getNRepeats 3

getTwoPair :: [Card] -> [Card]
getTwoPair xs = filter isInRanks xs
  where
    occurrences = countOccurrences xs
    ranksTwo = map fst (filter (\(_,times) -> times == 2) occurrences)
    isInRanks (Card rank _) = length ranksTwo == 2 && elem rank ranksTwo

getPair :: [Card] -> [Card]
getPair = getNRepeats 2

getHigh :: [Card] -> [Card]
getHigh xs = [maximum xs]

getFuncOfHand :: PokerHand -> [Card] -> [Card]
getFuncOfHand StraightFlush xs
  |(not . null) (getStraight xs) && (not . null) (getFlush xs) = xs
  |otherwise = []
getFuncOfHand FourOfAKind xs = getFour xs
getFuncOfHand FullHouse xs = getFullHouse xs
getFuncOfHand Flush xs = getFlush xs
getFuncOfHand Straight xs = getStraight xs
getFuncOfHand ThreeOfAKind xs = getThree xs
getFuncOfHand TwoPair xs = getTwoPair xs
getFuncOfHand Pair xs = getPair xs
getFuncOfHand HighCard xs = getHigh xs

getPokerHandAndCards :: [Card] -> (PokerHand, [Card])
getPokerHandAndCards xs = fromJust (find (not . null . snd) (map (\x -> (x,getFuncOfHand x xs)) [minBound..maxBound]))