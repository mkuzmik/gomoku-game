# Gomoku The Game

## Introduction
 It is an gomoku game implementation in haskell.

## How to play
 So as to play you need to have ghc haskell compiler installed. Clone this repository on your computer and run `runhaskell main.hs` in terminal.

## Task list
- [x] Create 'main' function.
- [ ] Create function that estimates the board's potential.

## Modules
- `main.hs` is the main module. It contains main fuction, which loops the game
- `Gomoku.hs` has the basic gomoku typeclasses and functions. Implementation of an board and pawns. Functions that are able to put pawn on board and evaluate whether the game is won (there are five the same pawns in one line).
