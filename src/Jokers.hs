module Jokers
  ( Joker(..)
  , allJokers
  , getDescription
  ) where
import Cards
    ( Card(..), Rank(R3, R2, R6, R9, R5, RA, R8, R7), Suit(Diamond, Spade) )
import PokerHands ( ChipsMult(..), PokerHand(Flush, ThreeOfAKind, FullHouse) )

data Joker = Joker String (ChipsMult -> (PokerHand, [Card]) -> ChipsMult)

instance Eq Joker where
  (Joker x _) == (Joker y __) = x == y

instance Show Joker where
  show (Joker name _) = name

allJokers :: [Joker]
allJokers =
  [ multSpades
  , multDiamonds
  , pirate
  , seven
  , twoDucks
  , flush
  , three
  , house
  , sixtySeven
  , fiftyOne
  , sport
  , theBite
  ]

getDescription :: Joker -> String
getDescription joker
  |joker == multSpades = "Cada carta de espadas pontuada dá +5 MULTI"
  |joker == multDiamonds = "Cada carta de ouros pontuada dá +5 MULTI"
  |joker == pirate = "Se a mão pontuar exatamente dois 9 dá +27 MULTI"
  |joker == seven = "Para cada 7 pontuado dá +7 MULTI"
  |joker == twoDucks = "Se a mão pontuar pelo menos dois 2 dá x2 MULTI"
  |joker == flush = "Se a mão for um flush dá x2 MULTI"
  |joker == three = "Se a mão for uma trinca dá x3 MULTI"
  |joker == house = "Se a mão for um full house dá x4 MULTI"
  |joker == sixtySeven = "Se a mão pontuar um 6 e um 7 dá +67 FICHAS"
  |joker == fiftyOne = "Se a mão pontuar um 5 e um Ás dá +51 FICHAS"
  |joker == sport = "Se a mão pontuar um 8 e um 7 dá +87 FICHAS"
  |joker == theBite = "Se a mão pontuar um 8 e um 3 dá +83 FICHAS, mas isso é só uma teoria"
  |otherwise = "Coringa inválido"

numOfSuit :: Suit -> [Card] -> Integer
numOfSuit suit xs = toInteger (length [s | (Card _ s) <- xs, s == suit])

numOfRank :: Rank -> [Card] -> Integer
numOfRank rank xs = toInteger (length [r | (Card r _) <- xs, r == rank])

hasRank :: Rank -> [Card] -> Bool
hasRank rank = any (\(Card r _) -> r == rank)

multSpades :: Joker
multSpades = Joker "Duelo de espadas" func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfSuit Spade hand)

multDiamonds :: Joker
multDiamonds = Joker "Devolvam nossos ouros" func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfSuit Diamond hand)

pirate :: Joker
pirate = Joker "O pirata" func
  where
    func (ChipsMult c m) (_, hand)
      |numOfRank R9 hand == 2 = ChipsMult c (m+27)
      |otherwise = ChipsMult c m

seven :: Joker
seven = Joker "Sorte a nossa" func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfRank R7 hand)

twoDucks :: Joker
twoDucks = Joker "Dois patinhos na lagoa" func
  where
    func (ChipsMult c m) (_, hand)
      |numOfRank R2 hand >= 2 = ChipsMult c (m*2)
      |otherwise = ChipsMult c m

flush :: Joker
flush = Joker "Vaso sanitário" func
  where
    func (ChipsMult c m) (pokerHand, _)
      |pokerHand == Flush = ChipsMult c (m*2)
      |otherwise = ChipsMult c m

three :: Joker
three = Joker "Três é demais" func
  where
    func (ChipsMult c m) (pokerHand, _)
      |pokerHand == ThreeOfAKind = ChipsMult c (m*3)
      |otherwise = ChipsMult c m

house :: Joker
house = Joker "Quatro paredes" func
  where
    func (ChipsMult c m) (pokerHand, _)
      |pokerHand == FullHouse = ChipsMult c (m*4)
      |otherwise = ChipsMult c m

sixtySeven :: Joker
sixtySeven = Joker "Seis sete" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R6 hand && hasRank R7 hand = ChipsMult (c+67) m
      |otherwise = ChipsMult c m

fiftyOne :: Joker
fiftyOne = Joker "Uma boa ideia" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R5 hand && hasRank RA hand = ChipsMult (c+51) m
      |otherwise = ChipsMult c m

sport :: Joker
sport = Joker "É do Sport" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R8 hand && hasRank R7 hand = ChipsMult (c+87) m
      |otherwise = ChipsMult c m

theBite :: Joker
theBite = Joker "A mordida" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R8 hand && hasRank R3 hand = ChipsMult (c+83) m
      |otherwise = ChipsMult c m