module Jokers
  ( Joker(..)
  , multClubs
  , multHearts
  , redSquid
  , twoDucks
  , fanta
  , sixtyNine
  , fiftyOne
  , theBite
  ) where
import Cards
import PokerHands

newtype Joker = Joker (ChipsMult -> (PokerHand, [Card]) -> ChipsMult)


numOfSuit :: Suit -> [Card] -> Integer
numOfSuit suit xs = toInteger (length [s | (Card _ s) <- xs, s == suit])

numOfRank :: Rank -> [Card] -> Integer
numOfRank rank xs = toInteger (length [r | (Card r _) <- xs, r == rank])

hasRank :: Rank -> [Card] -> Bool
hasRank rank = any (\(Card r _) -> r == rank)

multClubs :: Joker
multClubs = Joker func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfSuit Club hand)

multHearts :: Joker
multHearts = Joker func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfSuit Heart hand)

redSquid :: Joker
redSquid = Joker func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 13*numOfRank R9 hand)

twoDucks :: Joker
twoDucks = Joker func
  where
    func (ChipsMult c m) (_, hand)
      |numOfRank R2 hand >= 2 = ChipsMult c (m*2)
      |otherwise = ChipsMult c m

fanta :: Joker
fanta = Joker func
  where
    func (ChipsMult c m) (pokerHand, _)
      |pokerHand /= Straight = ChipsMult c (m+24)
      |otherwise = ChipsMult c m

sixtyNine :: Joker
sixtyNine = Joker func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R6 hand && hasRank R9 hand = ChipsMult (c+69) m
      |otherwise = ChipsMult c m

fiftyOne :: Joker
fiftyOne = Joker func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R5 hand && hasRank RA hand = ChipsMult (c+51) m
      |otherwise = ChipsMult c m

theBite :: Joker
theBite = Joker func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R8 hand && hasRank R3 hand = ChipsMult (c+83) m
      |otherwise = ChipsMult c m