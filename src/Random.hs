module Random
( shuffle
, getRandomItem
, getRandomItems
) where

import System.Random (randomRIO)

getRandomItem :: [item] -> IO item
getRandomItem xs = do
      randIdx <- randomRIO (0, length xs - 1)
      return (xs !! randIdx)

getRandomItems :: Int -> [item] -> IO [item]
getRandomItems n itemList = do
      scrambledItemList <- shuffle itemList
      return (take n scrambledItemList)

shuffle :: [a] -> IO[a]
shuffle [] = return []
shuffle xs = do
  randIdx <- randomRIO (0, length xs - 1)
  let (left, right) = splitAt randIdx xs
  case right of
      [] -> return xs 
      (picked:restOfRight) -> do
          rest <- shuffle (left ++ restOfRight)
          return (picked : rest)
