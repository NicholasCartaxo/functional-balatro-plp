module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (stdin, stdout)
import Data.Char ( intToDigit )
import GameLoop
    ( initialRoundGameState,
      playedPokerHandAndChipsMult,
      updateRoundGameState,
      RoundGameState(hands, jokers, hand, discards, score, targetScore, pokerHandChipsMult) )
import Cards ( Card(..), Suit(..) )
import PokerHands ( PokerHand, allPokerHands, getUpgradedPokerHandChipsMult )
import Jokers ( Joker, allJokers, getDescription )
import FullRoundLoop
    ( FullRoundState(currentRound, currentJokers, currentPokerHandChipsMult),
      changeJokerOrderFullRoundState,
      fullJokerFullRoundState,
      initialFullRoundState,
      nextFullRoundState,
      notFullJokerFullRoundState,
      upgradedPokerHandFullRoundState )
import GHC.IO.Handle ( hFlush, hSetEncoding )
import Random (getRandomItem, getRandomItems)
import System.Console.ANSI

getCharAndClean :: IO Char
getCharAndClean = do
    c <- getChar
    _ <- getLine
    return c

getColorCode :: Card -> String
getColorCode (Card _ suit) = 
  let color = case suit of 
                Heart -> Red
                Spade -> Blue
                Club -> Green
                Diamond -> Yellow
  in setSGRCode [SetColor Foreground Vivid color]

resetCode :: String 
resetCode = setSGRCode [Reset]

jokersColors :: [Color]
jokersColors = [Red, Green, Yellow, Blue, Magenta]

getJokerColorCode :: Int -> String
getJokerColorCode i =
  let color = jokersColors !! ((i - 1) `mod` length jokersColors)
  in setSGRCode [SetColor Foreground Vivid color]

renderCard :: Int -> (Card, Bool) -> String
renderCard i (card, selected) =
  "[" ++ show i ++ "] "
  ++ getColorCode card ++ show card ++ resetCode
  ++ if selected then " *" else ""

-- Renderiza a mão do jogador
renderHand :: [(Card, Bool)] -> String
renderHand xs =
  unlines (zipWith renderCard [1..] xs)

renderChipsMultTable :: RoundGameState -> [PokerHand] -> String
renderChipsMultTable st xs =
  unlines (map (renderChipsMult st) xs)

renderChipsMult :: RoundGameState -> PokerHand -> String
renderChipsMult st ph = show ph ++ " - " ++ show (pokerHandChipsMult st ph)

renderJoker :: Int -> Maybe Joker -> String
renderJoker i Nothing =
  "[" ++ show i ++ "] [ ]"

renderJoker i (Just joker) =
  "[" ++ show i ++ "] "
  ++ getJokerColorCode i
  ++ show joker 
  ++ resetCode
  ++ " — "
  ++ getDescription joker
  

renderJokers :: [Joker] -> String
renderJokers js =
  let slots = map Just js
  in unlines (zipWith renderJoker [1..5] slots)

clearTerminal :: IO ()
clearTerminal = putStr "\n\ESC[2J\ESC[H"

visibleLength :: String -> Int
visibleLength [] = 0
visibleLength ('\ESC':'[':xs) = visibleLength (drop 1 (dropWhile (/= 'm') xs))
visibleLength (_:xs) = 1 + visibleLength xs

padRight :: Int -> String -> String
padRight width s =
    let len = visibleLength s
        padding = max 0 (width - len)
    in s ++ replicate padding ' '

combineColumns :: Int -> [String] -> [String] -> String
combineColumns width left right =
    let maxH = max (length left) (length right)
        paddedLeft = left ++ replicate (maxH - length left) ""
        paddedRight = right ++ replicate (maxH - length right) ""
        merge l r = padRight width l ++ " | " ++ r
    in unlines (zipWith merge paddedLeft paddedRight)

printGameState :: RoundGameState -> IO ()
printGameState st = do
  clearTerminal
  
  let jokersBlock = 
        [ "===================================="
        , " CORINGAS"
        , "===================================="
        ]
        ++ lines (renderJokers (jokers st))
        ++ [""] 

  mapM_ putStrLn jokersBlock

  let statusBlock = 
        [ "===================================="
        , " MÃO ATUAL"
        , "===================================="
        ]
        ++ lines (renderHand (hand st))
        ++
        [ "------------------------------------"
        , "Fichas: " ++ show (score st) ++ " / " ++ show (targetScore st)
        , "Mão atual: " ++ show pokerHand
        , "FICHAS x MULTI: " ++ show chipsMult
        , " "
        , "Jogadas: " ++ show (hands st) ++ "    Descartes: " ++ show (discards st)
        , "------------------------------------"
        ]

  let commandsBlock = 
        [ "Comandos:"
        , " 1-8 = selecionar carta"
        , " q   = jogar mão"
        , " w   = descartar cartas"
        , " e   = ordenar por naipe"
        , " r   = ordenar por valor"
        ]

  let tableBlock = 
        [ "========================"
        , " MÃOS DE PÔQUER"
        , "========================"
        ]
        ++ lines (renderChipsMultTable st allPokerHands)

  let explanation = 
        [ "========================"
        , " OBJETIVO"
        , "========================"
        , "Jogue mãos de poker para pontuar fichas"
        , "Descarte cartas para tentar formar mãos melhores"
        , "Cada mão possui uma pontuação base"
        , "Cada carta pontua fichas com base em seu valor"
        , "Melhore suas mãos poker e ganhe coringas"
        , "Para pontuar mais fichas"
        ]

  let fullLeftColumn = statusBlock ++ [""] ++ commandsBlock
  let fullRightColumn = tableBlock ++ [""] ++ explanation

  putStrLn (combineColumns 40 fullLeftColumn fullRightColumn)
  putStrLn "--------------------------------------------------------------------"

  where
    (pokerHand, chipsMult) = playedPokerHandAndChipsMult st

fullJokersList :: [Joker] -> Bool
fullJokersList js = length js >= 5

isCharBetween :: Char -> Int -> Int -> Bool
isCharBetween c i j = c >= intToDigit i && c <= intToDigit j

switchJokersPosition :: FullRoundState -> IO FullRoundState
switchJokersPosition st =
  if length (currentJokers st) < 2
    then return st
  else do
    
    clearTerminal
    putStrLn "Você quer trocar a ordem dos jokers? Insira as posições dos jokers a serem trocados ou insira qualquer outra tecla para continuar.\n"
    putStrLn (renderJokers (currentJokers st))
    putStr "Joker 1: "
    hFlush stdout
    choice1 <- getCharAndClean

    if isCharBetween choice1 1 (length (currentJokers st)) then do
      putStr "Joker 2: "
      hFlush stdout
      choice2 <- getCharAndClean

      if isCharBetween choice2 1 (length (currentJokers st))
        then switchJokersPosition (changeJokerOrderFullRoundState choice1 choice2 st)
      else return st

    else do
      return st

pickJokerOrIncreasePokerHand :: FullRoundState -> IO FullRoundState
pickJokerOrIncreasePokerHand st = do

  clearTerminal

  putStrLn "\n🎉 Você atingiu a pontuação alvo!"
  putStrLn "\n=== Bônus da rodada ===\n"
  putStrLn "Para a próxima fase você pode escolher um dos bônus:"

  availableJokers <- getRandomItems 2 allJokers
  availablePokerHand <- getRandomItem allPokerHands

  putStrLn ("1: Joker " ++ show (head availableJokers))
  putStrLn ("2: Joker " ++ show (availableJokers !! 1))
  putStrLn ("3: Melhoria de mão " ++ show availablePokerHand ++ " - " ++ show (getUpgradedPokerHandChipsMult availablePokerHand (currentPokerHandChipsMult st availablePokerHand)))

  putStr "\nEscolha (1-3): "
  hFlush stdout
  choice <- getCharAndClean

  let
    fullJokersFlow = do
      putStrLn "\nSua lista de Jokers tá cheia, escolha qual joker você quer retirar (1-5): "
      putStr "Joker a sair: "
      hFlush stdout
      exitChoice <- getCharAndClean
      if exitChoice < '1' || exitChoice > '5' then fullJokersFlow
      else return (fullJokerFullRoundState choice exitChoice availableJokers st)

    resultFullRoundState
      |not (isCharBetween choice 1 3) = do
        pickJokerOrIncreasePokerHand st
      |choice == '3' = return (upgradedPokerHandFullRoundState availablePokerHand st)
      |fullJokersList (currentJokers st) = fullJokersFlow
      |otherwise = return (notFullJokerFullRoundState choice availableJokers st)

  resultFullRoundState


fullRoundLoop :: FullRoundState -> IO ()
fullRoundLoop st = do
  roundState <- initialRoundGameState st
  result <- gameLoop roundState
  if result then
    pickJokerOrIncreasePokerHand (nextFullRoundState st) >>= switchJokersPosition >>= fullRoundLoop
  else do
    putStrLn "\n❌ Acabaram as jogadas!"
    putStrLn "Fim de jogo!"
    putStrLn ("VOCÊ PERDEU, PARABÉNS! O MÁXIMO QUE TU ATINGIU FOI A RODADA " ++ show (currentRound st))

isWin :: RoundGameState -> Bool
isWin st = score st >= targetScore st

isOutOfMoves :: RoundGameState -> Bool
isOutOfMoves st = hands st <= 0


gameLoop :: RoundGameState -> IO Bool
gameLoop st = do

  printGameState st

  if isWin st then return True
  else if isOutOfMoves st then return False

  else do
    putStr "Escolha uma ação: "
    hFlush stdout

    action <- getCharAndClean

    let st' = updateRoundGameState action st
    gameLoop st'

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  fullRoundLoop initialFullRoundState