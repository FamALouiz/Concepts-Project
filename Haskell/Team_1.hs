-- Cell
type Cell = (Int,Int)

-- MyState
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

-- Up
up :: MyState -> MyState
up (S (x, y) listOfCells s state) = if x - 1 < 0 then Null else S (x - 1, y) listOfCells "up" (S (x, y) listOfCells s state)

-- Down
down :: MyState -> MyState
down (S (x, y) listOfCells s state) = if x +1 >3  then Null else S (x +1, y) listOfCells "down" (S (x, y) listOfCells s state)

-- Left
left :: MyState -> MyState
left (S (x, y) listOfCells s state) = if y-1  <0  then Null else S (x , y-1) listOfCells "left" (S (x, y) listOfCells s state)

-- Right
right :: MyState -> MyState
right (S (_,3) _ _ _)= Null
right (S (x,y) goldCells pAction pStates)=S (x,y+1) goldCells "right" (S (x,y) goldCells pAction pStates)

-- Remove Item helper function
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys

-- Dig
dig :: MyState -> MyState
dig (S cell listOfCells s state) =
    if any (==cell) listOfCells 
    then S cell (removeItem cell listOfCells) "dig" (S cell listOfCells s state) 
    else Null

-- Is Goal
isGoal :: MyState -> Bool
isGoal (S (_,_) [] _ _)=True
isGoal (S (_,_) listOfCells _ _)=False

--Next MyStates
nextMyStates :: MyState -> [MyState]
nextMyStates s= filter (/=Null) [up s,down s,left s,right s,dig s]

--Search 
search :: [MyState] -> MyState
search (state:tail) | isGoal state = state
                    | otherwise    = search (tail ++ (nextMyStates state))

--Construct Solution
constructSolution :: MyState -> [String]
constructSolution ( Null)= []
constructSolution (S _ _ action state)= if action == "" then (constructSolution state) else (constructSolution state)++[action]

--solve
solve :: Cell -> [Cell] -> [String]
solve root goldenCells
    |  checkGolden (root:goldenCells)= constructSolution (search([S root goldenCells "" Null]))
    | otherwise =  error "Oooh NOOOO, broo get the robot inside the grid!!!"


checkGolden []=True
checkGolden (cell:tail)= (isValidCell cell) && (checkGolden tail)

isValidCell (x,y)= (x<=3 && x>=0 && y<=3 && y>=0)
