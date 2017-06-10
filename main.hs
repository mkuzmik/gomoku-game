import Gomoku
import System.IO(hFlush, stdout)

main = game createStandardGomokuBoard

game :: Board -> IO ()
game board = do
  let won = isGameWon board
  if (won /= Null) then
    execute board "have won"
  else do
  print board
  putStr "input> "
  hFlush stdout
  cmd <- getLine
  execute board cmd

execute :: Board -> String -> IO ()
execute _ "quit" = putStrLn "Game is closed."
execute board "x" = do
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  game (insert board (X :: Field) (read x :: Int) (read y :: Int))
execute board "o" = do
  putStrLn "X position: "
  x <- getLine
  putStrLn "Y position: "
  y <- getLine
  game (insert board (O :: Field) (read x :: Int) (read y :: Int))
execute board "have won" =
  putStrLn (show board ++ "Player " ++ (show (isGameWon board)) ++ " have won!")
execute board _ = do
  putStrLn "Commands that you can use: \n \
  \ o    = put O on board \n \
  \ x    = put X on board \n \
  \ quit = quit the game "
  game board
