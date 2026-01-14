module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO (hSetEncoding, stdin, stdout, hFlush)
import Data.Char
import GameLoop
import Cards
import PokerHands
import Jokers
import FullRoundLoop
import Data.List (delete)
import Shop

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

fullJokersList :: [Joker] -> Bool
fullJokersList js = length js >= 5

pickJokerOrIncreasePokerHand :: FullRoundState -> IO FullRoundState
pickJokerOrIncreasePokerHand st = do
  putStrLn "\n=== Bônus da rodada  ===\n"
  putStrLn "Para a próxima fase você pode escolher um dos bônus:" -- Correção: putSrtLn -> putStrLn
  
  ((idxJ1, idxJ2), idxPokerHand, st') <- generateShopIdx st
  
  putStrLn ("[Joker 1] -> " ++ show (allJokers !! idxJ1))
  putStrLn ("[Joker 2] -> " ++ show (allJokers !! idxJ2))
  putStrLn ("[Melhoria de mão] -> " ++ show (allPokerHands !! idxPokerHand))

  putStrLn "1-2 : Recebe um coringa"
  putStrLn "3 - Recebe melhoria de mão"

  putStr "\nEscolha (1-3): "
  hFlush stdout
  choice <- getLine

  let jokerList = allJokers
  let pokerHandList = allPokerHands

  let chooseJoker :: Int -> IO FullRoundState
      chooseJoker idxJ =
        if not (fullJokersList (currentJokers st'))
          then return (nextFullRoundState (notFullJokerFullRoundState (intToDigit (idxJ + 1)) jokerList st'))
          else do
            putStrLn "\nSua lista de Jokers tá cheia, agora escolha qual joker você quer retirar (1-5): "
            hFlush stdout
            oldIdxStr <- getLine

            case oldIdxStr of
              [c] | c >= '1' && c <= '5' -> do
                return (nextFullRoundState (fullJokerFullRoundState (intToDigit (idxJ + 1)) c jokerList st'))
              _ -> do
                putStrLn "Indice inválido!"
                chooseJoker idxJ

  case choice of
    "1" -> chooseJoker idxJ1
    "2" -> chooseJoker idxJ2
    "3" -> do
          let ph = pokerHandList !! idxPokerHand
          return (nextFullRoundState (upgradedPokerHandFullRoundState ph st'))
    _ -> do 
      putStrLn "Opçãop inválida"  
      pickJokerOrIncreasePokerHand st'
  
fullRoundLoop :: FullRoundState -> IO ()
fullRoundLoop st = do
  result <- gameLoop (initialRoundGameState st)
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