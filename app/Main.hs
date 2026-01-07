module Main where
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.IO
import GameLoop
import Cards
import PokerHands
import Jokers

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


printGameState :: RoundGameState -> IO ()
printGameState st = do
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


-- Condições para finalizar rodada / jogo

isWin :: RoundGameState -> Bool
isWin st = score st >= targetScore st

isOutOfMoves :: RoundGameState -> Bool
isOutOfMoves st = hands st <= 0


-- Loop principal

gameLoop :: RoundGameState -> IO ()
gameLoop st = do

  printGameState st

  -- Aqui eu não sei como vai fazer pra retornar o gameloop pra proxima rodada
  -- talvez vamos ter que chamar o loop novamente com o updateRoundGameState @Cartaxo
  if isWin st then do
    putStrLn "\n🎉 Você atingiu a pontuação alvo!"
    return ()

  else if isOutOfMoves st then do
    putStrLn "\n❌ Acabaram as jogadas e/ou descartes!"
    putStrLn "Fim de jogo!"
    return ()

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

    if action == 'x' then do
      putStrLn "\nSaindo do jogo..."
      return ()
    else do
      let st' = updateRoundGameState action st 
      gameLoop st'


-- Main - rodar inicialmente o jogo
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  putStrLn "\n=== BALATRO - Card Game ==="
  gameLoop initialRoundGameState

