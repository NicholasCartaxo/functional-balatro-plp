# Functional Balatro (Haskell)

Este projeto é uma implementação funcional do jogo Balatro, desenvolvida em Haskell. O jogo desafia o jogador a criar mãos de poker, gerenciar coringas (Jokers) e superar pontuações crescentes em um estilo roguelike.

## Pré-requisitos

Para rodar este projeto, você precisa ter o Haskell Stack instalado.
- [Instalar Haskell Stack](https://docs.haskellstack.org/en/stable/)

## Como Rodar

### Windows (Importante)

Para garantir a visualização correta dos naipes e caracteres especiais no terminal do Windows, é **obrigatório** configurar a codificação para UTF-8 antes de executar o jogo.

1. Abra o PowerShell.
2. Execute os seguintes comandos em sequência:

```powershell
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
[Console]::InputEncoding  = [System.Text.Encoding]::UTF8
$OutputEncoding = [Console]::OutputEncoding
chcp 65001
```

3. Em seguida, compile e rode o jogo:
```
stack build
stack exec functional-balatro-plp-exe
```

### Linux / macOS
Em sistemas baseados em Unix, basta utilizar os comandos padrão do Stack:
```
stack build
stack exec functional-balatro-plp-exe
```

## Mecânicas do Jogo
O objetivo é vencer as "Blinds" (pontuações alvo) que aumentam a cada rodada.

## A Mão
  * O jogador recebe 8 cartas do baralho.
  * É possível selecionar ou desselecionar até 5 cartas para jogar ou descartar.
  * A seleção é feita digitando o número correspondente à carta (1 a 8).

## Coringas (Jokers)
  * O jogo possui um sistema de Slots de Coringas, onde você pode equipar até 5 coringas simultaneamente.
  * Os coringas modificam a pontuação ou dão bônus especiais.
  * Caso os slots estejam cheios, é possível substituir um coringa antigo por um novo adquirido.

## Progressão e Loja
Ao vencer uma rodada (atingir a pontuação necessária), o jogador tem a oportunidade de melhorar seu deck. Você poderá escolher entre:
 1. Dois Coringas Aleatórios: Para adicionar novos efeitos.
 2. Melhoria de Mão: Aumenta o nível (multiplicador e fichas base) de um tipo específico de mão de poker.

## Controles
Tecla  Ação
1-8  Selecionar / Desselecionar cartas (pelo índice)
q  Jogar mão (Confirma as cartas selecionadas)
w  Descartar (Troca as cartas selecionadas por novas)
e  Ordenar mão por Naipe
r  Ordenar mão por Valor

## Tabela de Pontuação
Cada mão possui um valor base de Fichas x Multiplicador. Estes valores podem ser aumentados durante a campanha. Abaixo, a tabela de valores iniciais:

Mão de Poker  Pontuação Base
Straight Flush  100 x 8
Quadra (Four of a Kind)  60 x 7
Full House  40 x 4
Flush  35 x 4
Sequência (Straight)  30 x 4
Trinca (Three of a Kind)  30 x 3
Dois Pares (Two Pair)  20 x 2
Par (Pair)  10 x 2
Carta Alta (High Card)  5 x 1
