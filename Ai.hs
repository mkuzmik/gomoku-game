module Ai (makeMove) where

import Gomoku
import System.Random
import System.IO.Unsafe

-- function that takes board and returns optimal move (x and y in range 1 -:- 19)
makeMove :: Board -> Field -> (Int,Int)
-- sample return (only to test main function)
-- makeMove _ _ = getRandomPosition 1 19
makeMove board player = getOptimalMoveFromDecisionTree $ buildDecisionTree board player 5

-- two numbers in touple from range
getRandomPosition :: Int -> Int -> (Int, Int)
getRandomPosition fir sec = (unsafePerformIO ( getStdRandom ( randomR(fir,sec))),unsafePerformIO ( getStdRandom ( randomR(fir,sec))))



-- Node (Field X or O) (Position) (Board after putting Filed into Position)
-- (Potential) (Roots)
data DecisionTree = Root Board Field [DecisionTree] | Node {currentPlayer::Field,
                                       move::(Int,Int),
                                       resultBoard::Board,
                                       potential::Int,
                                       children::[DecisionTree] } | Empty deriving Show

buildDecisionTree :: Board -> Field -> Int -> DecisionTree
buildDecisionTree board player height = Root board player $ buildNodes board player (0,0) height

buildNodes :: Board -> Field -> (Int,Int) -> Int -> [DecisionTree]
buildNodes board player nextPosition height
      | (fst nextPosition > 19) = buildNodes board player (0, (snd nextPosition) + 1) height
      | (snd nextPosition > 19) = []
      | otherwise =
            Node {
            currentPlayer = player,
            move = nextPosition,
            resultBoard = insert board player (fst nextPosition) (snd nextPosition),
            potential = 1, -- !!!! important section
            children =
                if (height == 0) then []
                else buildNodes (insert board player (fst nextPosition) (snd nextPosition)) (getNextPlayer player) (0,0) (height-1)
          }
            :(buildNodes board player ((fst nextPosition) + 1, snd nextPosition) height)

getNextPlayer :: Field -> Field
getNextPlayer X = O
getNextPlayer O = X
getNextPlayer _ = Null

-- this function needs to be done
getOptimalMoveFromDecisionTree :: DecisionTree -> (Int,Int)
getOptimalMoveFromDecisionTree _ = getRandomPosition 1 19
