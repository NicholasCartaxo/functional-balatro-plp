module Main where
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO
import GameLoop
import Cards
import PokerHands
import Jokers
import FullRoundLoop

import System.IO (hFlush, stdout)

-- Fizemos a primeira exibição das cartas, mas pretendemos inserir uma formatação melhorada, 
-- com () entre as cartas selecionadas
renderCard :: Int -> (Card, Bool) -> String
renderCard i (card, selected) =
  "[" ++ show i ++ "] "
  ++ show card
  ++ if selected then " *" else ""

-- renderiza a mão do jogador, aplicando a renderização de acda carta individualmente 
-- já com seus índices (utilizados em renderCard)
renderHand :: [(Card,Bool)] -> String
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
  -- transformamos a mão em um parágrafo de texto
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

pickJokerOrIncreasePokerHand :: FullRoundState -> IO FullRoundState
pickJokerOrIncreasePokerHand st = do
  putStrLn "ESCOLHE O JOKER PORRA"
  return (nextFullRoundState st)
  
fullRoundLoop :: FullRoundState -> IO()
fullRoundLoop st = do
  result <- gameLoop (initialRoundGameState st)
  if result then do
    nextFullRoundState <- (pickJokerOrIncreasePokerHand st)
    fullRoundLoop nextFullRoundState
  else do
    putStrLn ("VOCÊ PERDEU, PARABÉNS! O MÁXIMO QUE TU ATINGIU FOI A RODADA " ++ show (currentRound st))
    return()

-- Condições para finalizar rodada / jogo

isWin :: RoundGameState -> Bool
isWin st = score st >= targetScore st

isOutOfMoves :: RoundGameState -> Bool
isOutOfMoves st = hands st <= 0

-- Loop principal

gameLoop :: RoundGameState -> IO Bool
gameLoop st = do

  printGameState st

  -- Aqui eu não sei como vai fazer pra retornar o gameloop pra proxima rodada
  -- talvez vamos ter que chamar o loop novamente com o updateRoundGameState @Cartaxo
  if isWin st then do
    putStrLn "\n🎉 Você atingiu a pontuação alvo!"
    return True

  else if isOutOfMoves st then do
    putStrLn "\n❌ Acabaram as jogadas!"
    putStrLn "Fim de jogo!"
    return False

  else do
    putStr "Escolha uma ação: "
    --Quando a gente tava fazendo esse babado aqui, tava dando errado e não tava aparecendo
    -- Ai a gente achou a explicação de que as vezes haskell guarda as informaçõe em um buffer,
    -- e com esse comando abaixo, a gente consegue printar intantaneamente @Cartas
    hFlush stdout

    --leitura da entrada do usuário
    line <- getLine

    --tratamento, caso entrada da line esteja vazia
    let action =
          if null line then ' '
          else head line

    let st' = updateRoundGameState action st 
    gameLoop st'



-- Main - rodar inicialmente o jogo
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn "\n=== BALATRO - Card Game ==="
  fullRoundLoop initialFullRoundState


