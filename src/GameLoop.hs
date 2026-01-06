module GameLoop
  ( RoundGameState(..) 
  , initialRoundGameState
  , updateRoundGameState
  ) where
import Cards
import PokerHands
import Jokers
import Data.Char (digitToInt)
import Data.List (sortOn)

data RoundGameState = RoundGameState
  { hands :: Integer
  , discards :: Integer
  , hand :: [(Card,Bool)]
  , deck :: [Card]
  , score :: Integer
  , targetScore :: Integer
  , jokers :: [Joker]
  , pokerHandChipsMult :: (PokerHand -> ChipsMult)
  }

instance Show RoundGameState where
  show gameState = show (hand gameState)

initialRoundGameState :: RoundGameState
initialRoundGameState = RoundGameState
  { hands = 4
  , discards = 3
  , hand = [(card, False) | card <- take 8 fullDeck]
  , deck = drop 8 fullDeck
  , score = 0
  , targetScore = 300
  , jokers = []
  , pokerHandChipsMult = getInitialPokerHandChipsMult
  }

-- playGameLoopControls:
-- 1-8: select card at position
-- Q  : play selected hand
-- W  : discard selected hand
-- E  : sort by suit
-- R  : sort by rank

isValidDigit :: Char -> Bool
isValidDigit c = c >= '1' && c <= '8'

changeSelectedCard :: Int -> [(Card,Bool)] -> [(Card,Bool)]
changeSelectedCard i xs = left ++ ((card, not selected) : ys)
  where (left, (card, selected):ys) = splitAt (i-1) xs

separateSelected :: [(Card,Bool)] -> ([Card],[(Card,Bool)])
separateSelected xs = ([card | (card, selected) <- xs, selected], [(card,selected) | (card, selected) <- xs, not selected])

sortBySuit :: [(Card,Bool)] -> [(Card,Bool)]
sortBySuit = sortOn (\(Card _ suit, _) -> suit)

sortByRank :: [(Card,Bool)] -> [(Card,Bool)]
sortByRank = sortOn (\(Card rank _, _) -> rank)

updateRoundGameState :: Char -> RoundGameState -> RoundGameState
updateRoundGameState action gameState
  |isValidDigit action = gameState {hand = changeSelectedCard (digitToInt action) (hand gameState)}
  |action == 'e' = gameState {hand = sortBySuit (hand gameState)}
  |action == 'r' = gameState {hand = sortByRank (hand gameState)}
  |otherwise = gameState