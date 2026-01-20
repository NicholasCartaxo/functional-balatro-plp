module Main (main) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (stdin, stdout)
import Data.Char ( intToDigit )
import GameLoop
    ( initialRoundGameState,
      playedPokerHandAndChipsMult,
      updateRoundGameState,
      RoundGameState(hands, jokers, hand, discards, score, targetScore, pokerHandChipsMult) )
import Cards ( Card )
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

getCharAndClean :: IO Char
getCharAndClean = do
    c <- getChar
    _ <- getLine
    return c

renderCard :: Int -> (Card, Bool) -> String
renderCard i (card, selected) =
  "[" ++ show i ++ "] "
  ++ show card
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
  ++ show joker
  ++ " — "
  ++ getDescription joker

renderJokers :: [Joker] -> String
renderJokers js =
  let slots = map Just js
  in unlines (zipWith renderJoker [1..5] slots)

clearTerminal :: IO ()
clearTerminal = putStr "\n\ESC[2J\ESC[H"

printGameState :: RoundGameState -> IO ()
printGameState st = do
  clearTerminal
  putStrLn "\n===================================="
  putStrLn " CORINGAS"
  putStrLn "===================================="
  putStrLn (renderJokers (jokers st))

  putStrLn "\n===================================="
  putStrLn " MÃO ATUAL"
  putStrLn "===================================="

  putStrLn (renderHand (hand st))

  putStrLn "------------------------------------"
  putStrLn (
    "Pontuação: " ++ show (score st)
    ++ "         Objetivo: " ++ show (targetScore st))
  putStrLn ("Mão atual: " ++ show pokerHand)
  putStrLn ("Pontuação da mão atual: " ++ show chipsMult)
  putStrLn " "

  putStrLn (
    "Jogadas restantes: " ++ show (hands st)
    ++ "   Descartes: " ++ show (discards st))

  putStrLn "------------------------------------"

  putStrLn (renderChipsMultTable st allPokerHands)

  putStrLn "------------------------------------"
  putStrLn "Comandos:"
  putStrLn " 1-8 = selecionar carta"
  putStrLn " q   = jogar mão"
  putStrLn " w   = descartar cartas"
  putStrLn " e   = ordenar por naipe"
  putStrLn " r   = ordenar por valor"
  putStrLn "------------------------------------"

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