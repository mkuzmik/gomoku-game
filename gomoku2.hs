-- new version based on one-dimensional list

data Player =  Null | X | O deriving (Read)
instance Show Player where
  show (Null) = "_"
  show (X)    = "x"
  show (O)    = "o"

data Field = Player Int Int -- TypeOfField xLocation yLocation
instance Show Field where
  show (Field player _ _) = show player

data Board = Board [Field] Int -- ListOfFields BoardSize
instance Show Board where
  show (Board [] _)      = ""
  show (Board _ 0)       = ""
  show (Board ((Field player x y):xs) size) = 

showRow :: [Field] -> String
showRow [] = ""
showRow (x:xs) = show x ++ " " ++ showRow xs
