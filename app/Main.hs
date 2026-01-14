module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stdin, stdout, hFlush)
import Data.Char (digitToInt) 
import GameLoop
import Cards
import PokerHands
import Jokers
import FullRoundLoop

renderCard :: Int -> (Card, Bool) -> String
renderCard i (card, selected) =
  "[" ++ show i ++ "] "
  ++ show card
  ++ if selected then " *" else ""

-- Renderiza a mão do jogador
renderHand :: [(Card, Bool)] -> String
renderHand hand =
  unlines (zipWith renderCard [1..] hand)

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
  let slots = take 5 (map Just js ++ repeat Nothing)
  in unlines (zipWith renderJoker [1..5] slots)


printGameState :: RoundGameState -> IO ()
printGameState st = do
  putStr "\ESC[2J\ESC[H" 
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
  
  putStrLn " "

  putStrLn (
    "Jogadas restantes: " ++ show (hands st)
    ++ "   Descartes: " ++ show (discards st))

  putStrLn "------------------------------------"
  putStrLn "Comandos:"
  putStrLn " 1-8 = selecionar carta"
  putStrLn " q   = jogar mão"
  putStrLn " w   = descartar cartas"
  putStrLn " e   = ordenar por naipe"
  putStrLn " r   = ordenar por valor"
  putStrLn " x   = sair"
  putStrLn "------------------------------------"

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

rewardJokers :: [Joker]
rewardJokers = take 2 allJokers

pickJokerOrIncreasePokerHand :: FullRoundState -> IO FullRoundState
pickJokerOrIncreasePokerHand st = do
  putStrLn "\n=== Bônus da rodada  ===\n"
  putStrLn "Para a próxima fase você pode escolher um dos bônus:" -- Correção: putSrtLn -> putStrLn
  -- acredito que aqui vamos aexibir os 2 coringas aleatorios e a mao aleatori
  putStrLn "1 - Receber 2 Coringas"
  putStrLn "2 - Receber melhoria de mão"

  putStr "\nEscolha (1-2): "
  hFlush stdout
  choice <- getLine

  case choice of
    "1" -> -- implementar funcao q aplique os coringas e verifique se ta cheio 
    "2" -> -- implementar funcao q aplique a melhoria de mao aleatorio 

fullRoundLoop :: FullRoundState -> IO ()
fullRoundLoop st = do
  scrambledDeck <- shuffle fullDeck
  result <- gameLoop (initialRoundGameState st scrambledDeck)
  if result then do
    newSt <- pickJokerOrIncreasePokerHand st
    fullRoundLoop newSt
  else do
    putStrLn ("VOCÊ PERDEU, PARABÉNS! O MÁXIMO QUE TU ATINGIU FOI A RODADA " ++ show (currentRound st))
    return ()


isWin :: RoundGameState -> Bool
isWin st = score st >= targetScore st

isOutOfMoves :: RoundGameState -> Bool
isOutOfMoves st = hands st <= 0


gameLoop :: RoundGameState -> IO Bool
gameLoop st = do

  printGameState st

  if isWin st then do
    putStrLn "\n🎉 Você atingiu a pontuação alvo!"
    return True

  else if isOutOfMoves st then do
    putStrLn "\n❌ Acabaram as jogadas!"
    putStrLn "Fim de jogo!"
    return False

  else do
    putStr "Escolha uma ação: "
    hFlush stdout

    line <- getLine

    let action =
          if null line then ' '
          else head line

    let st' = updateRoundGameState action st 
    gameLoop st'

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn "\n=== BALATRO - Card Game ==="
  fullRoundLoop initialFullRoundState