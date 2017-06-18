-- Author: Mateusz Kuźmik
import Gomoku
import Ai
import System.IO(hFlush, stdout)


-- main function
main = do
  putStrLn "Welcome to Gomoku!"
  putStrLn "\nRemember!\n You must choose coordinates X (horizontal) and Y (vertical) from 1 to 19 range! \n"
  putStrLn "\tChoose game mode: \n \
           \ \t\t`c` = Custom \n \
           \ \t\t`s` = Single-Player \n \
           \ \t\t`m` = Multi-Player\n\n\t"
  modeStr <- getLine
  putStrLn "\n"
  chooseGameMode modeStr

chooseGameMode :: String -> IO ()
chooseGameMode "c" = customGame createStandardGomokuBoard
chooseGameMode "m" = multiPlayerGame createStandardGomokuBoard O
chooseGameMode "s" = singlePlayerGame createStandardGomokuBoard O
chooseGameMode _ = do
  putStrLn "\tWrong command!\n \tTry again..\n"
  main

-- SinglePlayer mode
-- player is O
-- computer is X
singlePlayerGame :: Board -> Field -> IO()
singlePlayerGame board nextPlayer = do
  let isWon = isGameWon board
  if (isWon /= Null) then
    execute board "have won"
  else do
    print board
    singlePlayerTurn board nextPlayer

singlePlayerTurn :: Board -> Field -> IO()
singlePlayerTurn board nextPlayer
  | nextPlayer == X = computerMove board X
  | otherwise = execute board "player move"

computerMove :: Board -> Field -> IO()
computerMove board field = do
  let pos = makeMove board field
  putStrLn "X player move!"
  if isFieldEmpty board pos == False then
    computerMove board field
  else
    singlePlayerGame (insert board (X :: Field) ((fst pos) :: Int) ((snd pos) :: Int)) O

-- Multiplayer mode
multiPlayerGame :: Board -> Field -> IO()
multiPlayerGame board nextPlayer = do
  let isWon = isGameWon board
  if (isWon /= Null) then
    execute board "have won"
  else do
    print board
    nextTurn board nextPlayer

nextTurn :: Board -> Field -> IO()
nextTurn board nextPlayer
      | nextPlayer == O = execute board "o move"
      | otherwise = execute board "x move"

-- custom game mode
customGame :: Board -> IO ()
customGame board = do
  let won = isGameWon board
  if (won /= Null) then
    execute board "have won"
  else do
  print board
  putStr "prompt> "
  hFlush stdout
  cmd <- getLine
  execute board cmd

execute :: Board -> String -> IO ()
execute _ "quit" = putStrLn "Game is closed."
execute board "x" = do
  putStrLn "X player move!"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  if isFieldEmpty board ((read x :: Int),(read y :: Int)) == False then
    execute board "x"
  else
    customGame (insert board (X :: Field) (read x :: Int) (read y :: Int))
execute board "o" = do
  putStrLn "O player move!"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  if isFieldEmpty board ((read x :: Int),(read y :: Int)) == False then
    execute board "o"
  else
    customGame (insert board (O :: Field) (read x :: Int) (read y :: Int))
execute board "x move" = do
  putStrLn "X player move!"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  if isFieldEmpty board ((read x :: Int),(read y :: Int)) == False then
    execute board "x move"
  else
    multiPlayerGame (insert board (X :: Field) (read x :: Int) (read y :: Int)) O
execute board "o move" = do
  putStrLn "O player move!"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  if isFieldEmpty board ((read x :: Int),(read y :: Int)) == False then
    execute board "o move"
  else
    multiPlayerGame (insert board (O :: Field) (read x :: Int) (read y :: Int)) X
execute board "player move" = do
  putStrLn "Your move!"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  if isFieldEmpty board ((read x :: Int),(read y :: Int)) == False then
    execute board "player move"
  else
    singlePlayerGame (insert board (O :: Field) (read x :: Int) (read y :: Int)) X
execute board "have won" =
  putStrLn (show board ++ "\n\nPlayer " ++ (show (isGameWon board)) ++ " have won!!!\n\n")
execute board _ = do
  putStrLn "\nCommands that you can use: \n \
  \ `o`    = put O on board \n \
  \ `x`    = put X on board \n \
  \ `quit` = quit the game\n"
  customGame board
