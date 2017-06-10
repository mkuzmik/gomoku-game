module Gomoku where

data Field =  Null | X | O deriving (Eq)
instance Show Field where
  show (Null) = "_"
  show (X)    = "x"
  show (O)    = "o"

-- eg. read "X" :: Field

data Board = Board {cells::[[Field]], size::Int} -- second arg is size
instance Show Board where
  show (Board (x:lst) size) = "\t" ++ showRow x ++ "\n" ++ show (Board lst size)
  show (Board [] _)      = ""

showRow :: [Field] -> String
showRow [] = ""
showRow (x:xs) = show x ++ " " ++ showRow xs

createBoard :: Int -> Field -> Board
createBoard size field = Board [ [field | x <- [1..size]] | y <- [1..size] ] size

createClearBoard :: Int -> Board
createClearBoard size = createBoard size Null

createStandardGomokuBoard :: Board
createStandardGomokuBoard = createClearBoard 19

getRow :: Board -> Int -> [Field]
getRow (Board rows _) x = rows!!x

getField :: Board -> Int -> Int -> Field
getField board x y = (getRow board x)!!y

insert :: Board -> Field -> Int -> Int -> Board
insert board@(Board _ size) field x y =
                                      Board [  [ (if (y == j && x == i) then
                                      (putIfPossible board field x y)
                                      else getField board j i)
                                      | i <- [0..size-1]] | j <- [0..size-1] ] size

putIfPossible :: Board -> Field -> Int -> Int -> Field
putIfPossible board field x y
      | ((getField board x y) == Null) = field
      | otherwise = getField board x y

-- isGameWon function gets Board and checks whether any player has already
-- won the game, then returns this player or Null if game is not won yet
--
-- it has to parse every row, column and slant into the list of Fields
-- then check it isFiveInRow function
isGameWon :: Board -> Field
isGameWon board
      | (checkRows board /= Null) = checkRows board
      | (checkCols board /= Null) = checkCols board
      | (checkSlants board /= Null) = checkSlants board
      | otherwise = Null

checkRows :: Board -> Field
checkRows (Board [] size) = Null
checkRows (Board (row:listOfRows) size)
      | ((isFiveInRow row) /= Null) = isFiveInRow row
      | otherwise = checkRows (Board listOfRows (size-1))

checkCols :: Board -> Field
checkCols board@(Board _ 0) = Null
checkCols board@(Board cells size)
      | ((isFiveInRow (aggregateColumn board (size-1))) /= Null) = isFiveInRow (aggregateColumn board (size-1))
      | otherwise = checkCols (Board cells (size-1))

checkSlants :: Board -> Field
checkSlants board@(Board _ 0) = Null
checkSlants board@(Board cells size)
      | ((isFiveInRow (aggregateSlant board (size-1) 1)) /= Null) = isFiveInRow (aggregateSlant board (size-1) 1)
      | ((isFiveInRow (aggregateSlant board (size-1) 1)) /= Null) = isFiveInRow (aggregateSlant board (size-1) 1)
      | otherwise = checkSlants (Board cells (size-1))

aggregateSlant :: Board -> Int -> Int -> [Field]
aggregateSlant board x slantNo
      | (slantNo == 0) = aggregateUpperSlant board x 0
      | otherwise = aggregateLowerSlant board x 0

aggregateUpperSlant :: Board -> Int -> Int -> [Field]
aggregateUpperSlant board@(Board cells size) slantNo tempCounter
      | (slantNo == size) = []
      | otherwise = (getField board slantNo tempCounter):(aggregateUpperSlant board (slantNo+1) (tempCounter+1))

aggregateLowerSlant :: Board -> Int -> Int -> [Field]
aggregateLowerSlant board@(Board cells size) slantNo tempCounter
      | (slantNo == size) = []
      | otherwise = (getField board tempCounter slantNo):(aggregateUpperSlant board (slantNo+1) (tempCounter+1))

aggregateColumn :: Board -> Int -> [Field]
aggregateColumn board@(Board [] size) no = []
aggregateColumn board@(Board (row:listOfRows) size) no =
  (row!!no):(aggregateColumn (Board listOfRows size) no)

-- gets list of Fields and returns fild that is 5 in row, otherwise Null
-- when there is five X and five O it returns X, but is special case
-- that should not occur
isFiveInRow :: [Field] -> Field
isFiveInRow fields
      | (searchForXFieldsInRow 5 X fields 5 == True) = X
      | (searchForXFieldsInRow 5 O fields 5 == True) = O
      | otherwise = Null

searchForXFieldsInRow :: Int -> Field -> [Field] -> Int -> Bool
searchForXFieldsInRow 0 _ _ _ = True
searchForXFieldsInRow _ _ [] _ = False
searchForXFieldsInRow amount field (x:xs) 1
      | (x == field) = True
      | otherwise = searchForXFieldsInRow amount field xs amount
searchForXFieldsInRow amount field (x:xs) counter
      | (x == field) = searchForXFieldsInRow amount field xs (counter-1)
      | otherwise = searchForXFieldsInRow amount field xs amount
