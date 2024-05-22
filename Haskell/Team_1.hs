-- Cell
type Cell = (Int,Int)

-- MyState
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)
 
up (S (x, y) listOfCells s state) = S (x, y + 1) listOfCells "up" (S (x, y) listOfCells s state)