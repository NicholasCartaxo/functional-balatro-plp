module GameLoop
  ( RoundGameState(..)
  , initialRoundGameState
  , updateRoundGameState
  , playedPokerHandAndChipsMult
  ) where
import Cards
import PokerHands
import Jokers
import FullRoundLoop
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
  , pokerHandChipsMult :: PokerHand -> ChipsMult
  }

instance Show RoundGameState where
  show gameState = show (hand gameState)

initialRoundGameState :: FullRoundState -> RoundGameState
initialRoundGameState fullRoundState = RoundGameState
  { hands = 4
  , discards = 3
  , hand = sortByRank [(card, False) | card <- take 8 fullDeck]
  , deck = drop 8 fullDeck
  , score = 0
  , targetScore = currentTargetScore fullRoundState 
  , jokers = currentJokers fullRoundState
  , pokerHandChipsMult = currentPokerHandChipsMult fullRoundState
  }

-- playGameLoopControls:
-- 1-8: select card at position
-- Q  : play selected hand
-- W  : discard selected hand
-- E  : sort by suit
-- R  : sort by rank

isValidDigit :: Char -> Bool
isValidDigit c = c >= '1' && c <= '8'

toggleAtPos :: Int -> [(Card,Bool)] -> [(Card,Bool)]
toggleAtPos i xs =
  case right of
    [] -> xs
    ((card, selected):ys) ->
      if selected || length selectedHand < 5 
      then left ++ ((card, not selected) : ys)
      else xs
  where
    (selectedHand, _) = separateSelected xs
    (left, right) = splitAt (i-1) xs

separateSelected :: [(Card,Bool)] -> ([Card],[(Card,Bool)])
separateSelected xs = ([card | (card, selected) <- xs, selected], [(card,selected) | (card, selected) <- xs, not selected])

drawNCards :: Int -> [(Card,Bool)] -> [Card] -> ([(Card,Bool)], [Card])
drawNCards n remainingHand currentDeck = (sortByRank (remainingHand ++ [(card,False) | card <- take n currentDeck]), drop n currentDeck)

sortBySuit :: [(Card,Bool)] -> [(Card,Bool)]
sortBySuit = sortOn (\(Card _ suit, _) -> suit)

sortByRank :: [(Card,Bool)] -> [(Card,Bool)]
sortByRank = sortOn (\(Card rank _, _) -> rank)

playedPokerHandAndChipsMult :: RoundGameState -> (PokerHand,ChipsMult)
playedPokerHandAndChipsMult state = (pokerHand, scoredChipsMult)
  where 
    selectedHand = fst (separateSelected (hand state))
    pokerHandAndScored = getPokerHandAndCards selectedHand
    pokerHand = fst pokerHandAndScored
    scoredChipsMult = foldl (\chipsMult (Joker _ func) -> func chipsMult pokerHandAndScored) (getChipsMultOfHand (pokerHandChipsMult state) selectedHand) (jokers state)

updateRoundGameState :: Char -> RoundGameState -> RoundGameState
updateRoundGameState action state
  |isValidDigit action = state {hand = toggleAtPos (digitToInt action) (hand state)}
  |action == 'q' && not (null selectedHand) && hands state > 0 = state
    { hands = hands state - 1
    , hand = nextHand
    , deck = nextDeck
    , score = score state + getScore handChipsMult
    }
  |action == 'w' && not (null selectedHand) && discards state > 0 = state
    { discards = discards state - 1
    , hand = nextHand
    , deck = nextDeck
    }
  |action == 'e' = state {hand = sortBySuit (hand state)}
  |action == 'r' = state {hand = sortByRank (hand state)}
  |otherwise = state
  where
    (selectedHand, remainingHand) = separateSelected (hand state)
    (nextHand, nextDeck) = drawNCards (length selectedHand) remainingHand (deck state)
    handChipsMult = snd (playedPokerHandAndChipsMult state)