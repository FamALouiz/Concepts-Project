-- Cell
type Cell = (Int,Int)

-- MyState
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

-- Up
up :: MyState -> MyState
up (S (x, y) listOfCells s state) = if x - 1 < 0 then Null else S (x - 1, y) listOfCells "up" (S (x, y) listOfCells s state)


-- Remove Item helper function
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Dig
dig :: MyState -> MyState
dig (S cell listOfCells s state) = if any (==cell) listOfCells 
    then S cell (removeItem cell listOfCells) "dig" (S cell listOfCells s state) 
    else Null