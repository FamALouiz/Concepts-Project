-- Cell
type Cell = (Int,Int)

-- MyState
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

-- Up
up :: MyState -> MyState
up (S (x, y) listOfCells s state) = if x - 1 < 0 then Null else S (x - 1, y) listOfCells "up" (S (x, y) listOfCells s state)