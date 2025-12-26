module Cards
  ( Rank(..)
  , rankValue
  , Suit(..)
  , Card(..)
  ) where

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
