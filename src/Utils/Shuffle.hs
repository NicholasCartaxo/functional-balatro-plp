module Utils.Shuffle (shuffle) where

import System.Random (randomRIO)

shuffle :: [a] -> IO[a]
shuffle [] = return []
shuffle xs = do
  randIdx <- randomRIO(0, length xs - 1)
  let (left, (picked:right)) = splitAt randIdx xs
  rest <- shuffle (left ++ right)
  return (picked : rest)
