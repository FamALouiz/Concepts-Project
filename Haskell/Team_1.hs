-- Cell
type Cell = (Int,Int)

-- MyState
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)
<<<<<<< Updated upstream

-- Up
up :: MyState -> MyState
up (S (x, y) listOfCells s state) = if x - 1 < 0 then Null else S (x - 1, y) listOfCells "up" (S (x, y) listOfCells s state)

--down
down :: MyState -> MyState
down (S (x, y) listOfCells s state) = if x +1 >3  then Null else S (x +1, y) listOfCells "down" (S (x, y) listOfCells s state)

--left
left :: MyState -> MyState
left (S (x, y) listOfCells s state) = if y-1  <0  then Null else S (x , y-1) listOfCells "left" (S (x, y) listOfCells s state)



-- Remove Item helper function
removeItem _ [] = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Dig
dig :: MyState -> MyState
dig (S cell listOfCells s state) = if any (==cell) listOfCells 
    then S cell (removeItem cell listOfCells) "dig" (S cell listOfCells s state) 
    else Null
right :: MyState -> MyState

right (S (_,3) _ _ _)= Null
right (S (x,y) goldCells pAction pStates)=(S (x,y+1) goldCells "right" (S (x,y) goldCells pAction pStates))

isGoal :: MyState -> Bool

isGoal (S (_,_) [] _ _)=True
isGoal (S (_,_) listOfCells _ _)=False


>>>>>>> Stashed changes
