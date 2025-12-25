module Cards where
import Data.List
import Data.Maybe

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq, Ord, Enum, Bounded)
instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = "10"; show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

rankValue :: Rank -> Int
rankValue R2  = 2
rankValue R3  = 3
rankValue R4  = 4
rankValue R5  = 5
rankValue R6  = 6
rankValue R7  = 7
rankValue R8  = 8
rankValue R9  = 9
rankValue R10 = 10
rankValue RJ  = 10
rankValue RQ  = 10
rankValue RK  = 10
rankValue RA = 11

data Suit = Spade | Heart | Club | Diamond
  deriving (Eq, Ord)
instance Show Suit where
  show Spade   = "\x2660" -- unicode characters for suits
  show Heart   = "\x2665"
  show Diamond = "\x2666"
  show Club    = "\x2663"

data Card = Card Rank Suit
  deriving (Eq, Show, Ord)

data PokerHand = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
  deriving (Eq, Show, Ord, Enum, Bounded)

getStraight :: [Card] -> [Card]
getStraight xs
  |length xs /= 5 = []
  |otherwise = if straightLoop (sort xs) then xs else []
  where
    straightLoop [x] = True
    straightLoop [Card R5 _, Card RA _] = True
    straightLoop (Card x _:Card y __:xs) =
      (y == succ x) && straightLoop (Card y __:xs)

getFlush :: [Card] -> [Card]
getFlush xs
  |length xs /= 5 = []
  |otherwise = if flushLoop xs then xs else []
  where
    flushLoop [x] = True
    flushLoop (Card _ x:Card __ y:xs) =
      (x == y) && flushLoop (Card __ y:xs)

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