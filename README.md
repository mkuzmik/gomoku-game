# Gomoku The Game

## Introduction
 It is an gomoku game implementation in haskell.

## How to play
 So as to play you need to have ghc haskell compiler installed. Clone this repository on your computer and run `runhaskell main.hs` in terminal. Then just follow commands.

## Task list
- [x] Create 'main' function.
- [x] Create game mode selection.
- [ ] Create AI module
- [ ] Enable singleplayer mode.
- [ ] Report exception when typed player tries to put field out of board or onto existing field.

## Modules
- `main.hs` is the main module. It contains main fuction, which loops the game
- `Gomoku.hs` has the basic gomoku typeclasses and functions. Implementation of an board and pawns. Functions that are able to put pawn on board and evaluate whether the game is won (there are five the same pawns in one line).
- `Ai.hs` is an module which implements algorithm for singleplayer mode. The main function is `makeMove :: Board -> Field -> (x,y)`
