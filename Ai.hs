module Ai (makeMove) where

import Gomoku

-- function that takes board and returns move (x and y in range 1 -:- 19)
makeMove :: Board -> Field -> (Int,Int)
makeMove _ _ = (1,1)
