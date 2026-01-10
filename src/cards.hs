module Cards
  ( Rank(..)
  , rankValue
  , Suit(..)
  , Card(..)
  , fullDeck
  ) where

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq, Ord, Enum, Bounded)
instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = "10"; show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

rankValue :: Rank -> Integer
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
  deriving (Eq, Ord)
instance Show Card where
  show (Card r s) = show r ++ show s

fullDeck :: [Card]
fullDeck = [Card R2 Spade, Card R2 Heart, Card R2 Diamond, Card R2 Club,
        Card R3 Spade, Card R3 Heart, Card R3 Diamond, Card R3 Club,
        Card R4 Spade, Card R4 Heart, Card R4 Diamond, Card R4 Club,
        Card R5 Spade, Card R5 Heart, Card R5 Diamond, Card R5 Club,
        Card R6 Spade, Card R6 Heart, Card R6 Diamond, Card R6 Club,
        Card R7 Spade, Card R7 Heart, Card R7 Diamond, Card R7 Club,
        Card R8 Spade, Card R8 Heart, Card R8 Diamond, Card R8 Club,
        Card R9 Spade, Card R9 Heart, Card R9 Diamond, Card R9 Club,
        Card R10 Spade, Card R10 Heart, Card R10 Diamond, Card R10 Club,
        Card RJ Spade, Card RJ Heart, Card RJ Diamond, Card RJ Club,
        Card RQ Spade, Card RQ Heart, Card RQ Diamond, Card RQ Club,
        Card RK Spade, Card RK Heart, Card RK Diamond, Card RK Club,
        Card RA Spade, Card RA Heart, Card RA Diamond, Card RA Club
        ]