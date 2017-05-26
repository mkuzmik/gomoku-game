data Field =  Null | X | O deriving (Read)
instance Show Field where
  show (Null) = "_"
  show (X)    = "x"
  show (O)    = "o"

data Board = Board [[Field]]
instance Show Board where
  show (Board (x:lst)) = showRow x ++ "\n" ++ show (Board lst)
  show (Board [])      = ""

showRow :: [Field] -> String
showRow [] = ""
showRow (x:xs) = show x ++ showRow xs
