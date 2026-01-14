module Jokers
  ( Joker(..)
  , allJokers
  , getDescription
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

data Joker = Joker String (ChipsMult -> (PokerHand, [Card]) -> ChipsMult)

instance Eq Joker where
  (Joker x _) == (Joker y __) = x == y

instance Show Joker where
  show (Joker name _) = name

allJokers :: [Joker]
allJokers =
  [ multClubs
  , multHearts
  , redSquid
  , twoDucks
  , fanta
  , sixtyNine
  , fiftyOne
  , theBite
  ]

getDescription :: Joker -> String
getDescription joker
  |joker == multClubs = "Cada carta de paus pontuada dá +5 MULT"
  |joker == multHearts = "Cada carta de ouros pontuada dá +5 MULT"
  |joker == redSquid = "Cada 9 pontuado dá +13 MULT"
  |joker == twoDucks = "Se a mão pontuar pelo menos dois 2 dá x2 MULT"
  |joker == fanta = "Se a mão não for uma Sequência dá +24 MULT"
  |joker == sixtyNine = "Se a mão pontuar um 6 e um 9 dá +69 CHIPS"
  |joker == fiftyOne = "Se a mão pontuar um 5 e um Ás dá +51 CHIPS"
  |joker == theBite = "Se a mão pontuar um 8 e um 3 dá +83 CHIPS, mas isso é só uma teoria"
  |otherwise = "Coringa inválido"

numOfSuit :: Suit -> [Card] -> Integer
numOfSuit suit xs = toInteger (length [s | (Card _ s) <- xs, s == suit])

numOfRank :: Rank -> [Card] -> Integer
numOfRank rank xs = toInteger (length [r | (Card r _) <- xs, r == rank])

hasRank :: Rank -> [Card] -> Bool
hasRank rank = any (\(Card r _) -> r == rank)

multClubs :: Joker
multClubs = Joker "Botar os paus na mesa" func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfSuit Club hand)

multHearts :: Joker
multHearts = Joker "Devolvam nossos ouros" func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 5*numOfSuit Heart hand)

redSquid :: Joker
redSquid = Joker "Lula vermelha" func
  where
    func (ChipsMult c m) (_, hand) = ChipsMult c (m + 13*numOfRank R9 hand)

twoDucks :: Joker
twoDucks = Joker "Dois patinhos na lagoa" func
  where
    func (ChipsMult c m) (_, hand)
      |numOfRank R2 hand >= 2 = ChipsMult c (m*2)
      |otherwise = ChipsMult c m

fanta :: Joker
fanta = Joker "Essa coca é fanta" func
  where
    func (ChipsMult c m) (pokerHand, _)
      |pokerHand /= Straight = ChipsMult c (m+24)
      |otherwise = ChipsMult c m

sixtyNine :: Joker
sixtyNine = Joker "Meia nove" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R6 hand && hasRank R9 hand = ChipsMult (c+69) m
      |otherwise = ChipsMult c m

fiftyOne :: Joker
fiftyOne = Joker "Uma boa ideia" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R5 hand && hasRank RA hand = ChipsMult (c+51) m
      |otherwise = ChipsMult c m

theBite :: Joker
theBite = Joker "A mordida" func
  where
    func (ChipsMult c m) (_, hand)
      |hasRank R8 hand && hasRank R3 hand = ChipsMult (c+83) m
      |otherwise = ChipsMult c m