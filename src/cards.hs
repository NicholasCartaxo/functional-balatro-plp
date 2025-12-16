data Rank = RA | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK 
  deriving (Eq, Ord)

instance Show Rank where
  show RA  = "A";
  show R2  = "2"; show R3  = "3"; show R4  = "4"; show R5  = "5";
  show R6  = "6"; show R7  = "7"; show R8  = "8"; show R9  = "9";
  show R10 = [toEnum 0x2491] :: String; -- ten in only one character
  show RJ  = "J"; show RQ  = "Q"; show RK  = "K";

instance Value Rank where
  value RA = 11;
  show R2  = 2; show R3  = 3; show R4  = 4; show R5  = 5;
  show R6  = 6; show R7  = 7; show R8  = 8; show R9  = 9;
  show R10 = 10;
  show RJ  = 10; show RQ  = 10; show RK  = 10;


data Suit = Spade | Heart | Club | Diamond 
  deriving (Eq)

instance Show Suit where
  show Spade   = [toEnum 0x2660] :: String -- unicode characters for suits
  show Heart   = [toEnum 0x2665] :: String
  show Diamond = [toEnum 0x2666] :: String 
  show Club    = [toEnum 0x2663] :: String

data Card = Card Rank Suit
  deriving (Eq, Show, Ord)

data PokerHand = StraightFlush | FourOfAKind | FullHouse | Flush | Straight | ThreeOfAKind | TwoPair | Pair | HighCard
  deriving (Eq, Ord)