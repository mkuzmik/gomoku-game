module Ai (makeMove) where

import Gomoku
import System.Random
import System.IO.Unsafe

-- function that takes board and returns optimal move (x and y in range 1 -:- 19)
makeMove :: Board -> Field -> (Int,Int)
-- sample return (only to test main function)
makeMove _ _ = getRandomPosition 1 19

-- two numbers in touple from range
getRandomPosition :: Int -> Int -> (Int, Int)
getRandomPosition fir sec = (unsafePerformIO ( getStdRandom ( randomR(fir,sec))),unsafePerformIO ( getStdRandom ( randomR(fir,sec))))
