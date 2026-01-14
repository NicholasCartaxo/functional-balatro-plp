module Shop
  ( ShopOffer(..)
  , drawIndex
  , generateShopIdx
  ) where

import Data.List (delete)
import PokerHands
import Jokers
import FullRoundLoop

data ShopOffer = ShopOffer
  { offerJ1 :: Integer
  , offerJ2 :: Integer
  , offerB  :: Integer
  } deriving (Show)

drawIndex :: [Int] -> IO (Int, [Int])
drawIndex [] = error "Lista vazia!"
drawIndex (x:xs) = return (x, xs)

generateShopIdx :: FullRoundState -> IO ((Int, Int), Int, FullRoundState)
generateShopIdx st = do
  (idJ1, list) <- drawIndex (jokerList st)
  (idJ2, list2) <- drawIndex list
  (idPH, list3) <- drawIndex (pokerHandList st)

  let st' = st { jokerList = list2, pokerHandList = list3 }
  return ((idJ1, idJ2), idPH, st')