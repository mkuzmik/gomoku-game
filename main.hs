import Gomoku
import System.IO(hFlush, stdout)



main = do
  putStrLn "Choose game mode: \n \
           \ custom / singleplayer / multiplayer\n\n"
  modeStr <- getLine
  chooseGameMode modeStr

chooseGameMode :: String -> IO ()
chooseGameMode "custom" = customGame createStandardGomokuBoard
chooseGameMode "multiplayer" = multiPlayerGame createStandardGomokuBoard O
chooseGameMode "singleplayer" = do
  putStrLn "Not accessible yet !"
  main
chooseGameMode _ = do
  putStrLn "Wrong command!\n Try again..\n"
  main

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
  putStrLn "X player move !"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  customGame (insert board (X :: Field) (read x :: Int) (read y :: Int))
execute board "o" = do
  putStrLn "O player move !"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  customGame (insert board (O :: Field) (read x :: Int) (read y :: Int))
execute board "x move" = do
  putStrLn "X player move !"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  multiPlayerGame (insert board (X :: Field) (read x :: Int) (read y :: Int)) O
execute board "o move" = do
  putStrLn "O player move !"
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  multiPlayerGame (insert board (O :: Field) (read x :: Int) (read y :: Int)) X
execute board "have won" =
  putStrLn (show board ++ "Player " ++ (show (isGameWon board)) ++ " have won!")
execute board _ = do
  putStrLn "Commands that you can use: \n \
  \ o    = put O on board \n \
  \ x    = put X on board \n \
  \ quit = quit the game "
  customGame board
