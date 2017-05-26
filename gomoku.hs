import System.Random
import System.IO.Unsafe


data Field =  Null | X | O deriving (Read)
instance Show Field where
  show (Null) = "_"
  show (X)    = "x"
  show (O)    = "o"

data Board = Board [[Field]]
instance Show Board where
  show (Board (x:lst)) = "\t" ++ showRow x ++ "\n" ++ show (Board lst)
  show (Board [])      = ""

showRow :: [Field] -> String
showRow [] = ""
showRow (x:xs) = show x ++ " " ++ showRow xs

createBoard :: Int -> Field -> Board
createBoard size field = Board [ [field | x <- [1..size]] | y <- [1..size] ]

createClearBoard :: Int -> Board
createClearBoard size = createBoard size Null

nextTurn :: Board -> Int -> Int -> Field -> Board
nextTurn board x y field
        | x == 0 && y == 0 =

-- TODO: generate board with random Fields
-- createRandomBoard :: Int -> Board
-- createRandomBoard size = createBoard size (randomField randomFieldGenerator)
--
-- randomField :: (Eq a, Num a) => a -> Field
-- randomField gen
--       | gen == 0 = Null
--       | gen == 1 = X
--       | gen == 2 = O
--
-- randomFieldGenerator :: Int
-- randomFieldGenerator = unsafePerformIO (getStdRandom (randomRIO (0, 2)))
