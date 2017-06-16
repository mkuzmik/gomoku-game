module Ai (makeMove) where

import Gomoku
import System.Random
import System.IO.Unsafe

-- function that takes board and returns optimal move (x and y in range 1 -:- 19)
makeMove :: Board -> Field -> (Int,Int)
-- sample return (only to test main function)
-- makeMove _ _ = getRandomPosition 1 19
makeMove board player = getOptimalMoveFromDecisionTree $ buildDecisionTree board player 3

-- two numbers in touple from range
getRandomPosition :: Int -> Int -> (Int, Int)
getRandomPosition fir sec = (unsafePerformIO ( getStdRandom ( randomR(fir,sec))),unsafePerformIO ( getStdRandom ( randomR(fir,sec))))



-- Node (Field X or O) (Position) (Board after putting Filed into Position)
-- (Potential) (Roots)
data DecisionTree = Root Board Field [DecisionTree] | Node {currentPlayer::Field,
                                       move::(Int,Int),
                                       resultBoard::Board,
                                       potential::Double,
                                       aggregatedPotential::Double,
                                       children::[DecisionTree] } | Empty deriving Show

buildDecisionTree :: Board -> Field -> Int -> DecisionTree
buildDecisionTree board player height = Root board player $ buildNodes board player (1,1) height

buildNodes :: Board -> Field -> (Int,Int) -> Int -> [DecisionTree]
buildNodes board player nextPosition height
      | (fst nextPosition > 19) = buildNodes board player (1, (snd nextPosition) + 1) height
      | (snd nextPosition > 19) = []
      | isFieldEmpty board nextPosition =
            Node {
            currentPlayer = player,
            move = nextPosition,
            resultBoard = insert board player (fst nextPosition) (snd nextPosition),
            potential = rateMyBoard (insert board player (fst nextPosition) (snd nextPosition)) player, -- !!!! important section
            aggregatedPotential = if (height == 0) then rateMyBoard (insert board player (fst nextPosition) (snd nextPosition)) player else 0,
            children =
                if (height == 0) then []
                else buildNodes (insert board player (fst nextPosition) (snd nextPosition)) (getNextPlayer player) (1,1) (height-1)
          }
            :(buildNodes board player ((fst nextPosition) + 1, snd nextPosition) height)
      | otherwise = buildNodes board player (((fst nextPosition) + 1),(snd nextPosition)) height

getNextPlayer :: Field -> Field
getNextPlayer X = O
getNextPlayer O = X
getNextPlayer _ = Null

-- this function needs to be done
getOptimalMoveFromDecisionTree :: DecisionTree -> (Int,Int)
getOptimalMoveFromDecisionTree _ = getRandomPosition 1 19

-- function rateMyBoard rates board for given player
-- when your opponent wins potential is 0
-- when you win the board potential is 1.0
-- needs to be imporved
rateMyBoard :: Board -> Field -> Double
rateMyBoard board player
      | isGameWon board == player = 1.0
      | isGameWon board == getNextPlayer player = 0.0
      | otherwise = 0.5
