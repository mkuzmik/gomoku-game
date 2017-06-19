# Gomoku The Game

## Introduction
 It is an gomoku game implementation in haskell.

## How to play
 So as to play you need to have ghc haskell compiler installed. Clone this repository on your computer and run `runhaskell main.hs` in terminal. Then just follow commands.

## Game modes
 Gomoku The Game has three modes:
 - **Single Player** is a mode in which you are an 'O' and computer is an 'X'. Every turn you are asked only of coordinates (X and Y) of your next move.
 - **Multi Player**, using this mode you can play with other person.
 - **Custom** mode is especially for testing purposes. You can choose which figure you want to choose and then put the coordinates. When you run Custom mode you will get a prompt where yo are supposed to put a command. If you do not know any type empty line, you will get a hint.

## Single Player
 Original conception was that computer is choosing next move by analysis a tree of all possible moves. This tree (called DecisionTree in my project in Ai.hs module) is already implemented, but it is not working properly, therefore computer player choose random position on a board yet.

## Task list
- [x] Create `Gomoku.hs`module, which implements basic game mechanisms.
- [x] Create 'main' function in `main.hs`, which makes you able to play Gomoku in command line interface.
- [x] Create game mode selection (singleplayer/multiplayer/custom).
- [x] Create `Ai.hs` module.
- [x] Create function which makes move on random field.
- [x] Implement a Decision Tree. It contains every possible movement in n rounds.
- [x] Implement a function that is able to rate potential of the Board.  
- [ ] Connect single player mode with choosing right path using Decision Tree.

## Modules
- `main.hs` is the main module. It contains main fuction, which loops the game
- `Gomoku.hs` has the basic gomoku typeclasses and functions. Implementation of an board and pawns. Functions that are able to put pawn on board and evaluate whether the game is won (there are five the same pawns in one line).
- `Ai.hs` is an module which implements algorithm for singleplayer mode. The main function is `makeMove :: Board -> Field -> (x,y)`. It also contains implementation of DecisionTree and functions related to it.
