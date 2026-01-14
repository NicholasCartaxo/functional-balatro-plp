module FullRoundLoop
  ( FullRoundState(..)
  , initialFullRoundState
  , nextFullRoundState
  , upgradedPokerHandFullRoundState
  , notFullJokerFullRoundState
  , fullJokerFullRoundState
  , changeJokerOrderFullRoundState
  ) where
import PokerHands
import Jokers
import Data.Char (digitToInt)

data FullRoundState = FullRoundState
  { currentTargetScore :: Integer
  , currentRound :: Integer
  , currentJokers :: [Joker]
  , currentPokerHandChipsMult :: PokerHand -> ChipsMult
  , jokerList :: [Int]
  , pokerHandList :: [Int]
  }

initialFullRoundState :: FullRoundState
initialFullRoundState = FullRoundState
  { currentTargetScore = 300
  , currentRound = 1
  , currentJokers = []
  , currentPokerHandChipsMult = getInitialPokerHandChipsMult
  , jokerList = [0 .. length allJokers - 1]
  , pokerHandList = [0 .. length allPokerHands - 1]
  }

nextFullRoundState :: FullRoundState -> FullRoundState
nextFullRoundState state = FullRoundState
  { currentTargetScore = currentTargetScore state * 2
  , currentRound = currentRound state + 1
  , currentJokers = currentJokers state
  , currentPokerHandChipsMult = currentPokerHandChipsMult state
  , jokerList = jokerList state
  , pokerHandList = pokerHandList state
  }

upgradedPokerHandFullRoundState :: PokerHand -> FullRoundState -> FullRoundState
upgradedPokerHandFullRoundState pokerHandUpgrade state = state {currentPokerHandChipsMult = upgradedPokerHandChipsMult}
  where
    upgradedPokerHandChipsMult pokerHand
      |pokerHand == pokerHandUpgrade = getUpgradedPokerHandChipsMult pokerHand (currentPokerHandChipsMult state pokerHand)
      |otherwise = currentPokerHandChipsMult state pokerHand

notFullJokerFullRoundState :: Char -> [Joker] -> FullRoundState -> FullRoundState
notFullJokerFullRoundState idx jokers state = state {currentJokers = selectedJoker : currentJokers state}
  where
    selectedJoker = jokers !! (digitToInt idx - 1 )

fullJokerFullRoundState :: Char -> Char -> [Joker] -> FullRoundState -> FullRoundState
fullJokerFullRoundState idxNew idxOld jokers state = state {currentJokers = newCurrentJokers}
  where
    newJoker = jokers !! (digitToInt idxNew - 1 )
    newCurrentJokers = 
      case right of
        [] -> currentJokers state
        (_:ys) -> left ++ (newJoker : ys)
      where
        (left, right) = splitAt (digitToInt idxOld-1) (currentJokers state)

changeJokerOrderFullRoundState :: Char -> Char -> FullRoundState -> FullRoundState
changeJokerOrderFullRoundState idx1 idx2 state = state {currentJokers = swappedJokers (digitToInt idx1) (digitToInt idx2) (currentJokers state)}
  where
    swappedJokers i j xs
      | i == j    = currentJokers state                    
      | i > j     = swappedJokers j i xs   
      | otherwise = left ++ [elemJ] ++ middle ++ [elemI] ++ right
          where
            elemI = xs !! i
            elemJ = xs !! j
            left = take i xs
            middle = take (j - i - 1) (drop (i + 1) xs)
            right = drop (j + 1) xs