import System.Random

data Location = Location { x :: Int
                         , y :: Int } deriving Eq

data Path = Path { from :: Location
                 , to :: Location } deriving Eq

data Cell = Cell { location :: Location
                 , neighbours :: [Location] } deriving (Eq,Show)

data Globals = Globals { width :: Int
                       , height :: Int
                       , start :: Location
                       , end :: Location
                       , begin :: Location
                       , r :: [Int] } deriving Show

data Maze = Maze { cells :: [Cell]
                 , paths :: [Path]
                 , stack :: [Location]
                 , visited :: [Location]
                 , counter :: Int } deriving Show

data Display = Display { rows :: [DisplayRow] }

data DisplayRow = DisplayRow { cols :: [DisplayCol] }

data DisplayCol = DisplayCol { element :: ElementType }

data ElementType = Wall | Space | Start | End

instance Show Display where
  show d = "\n" ++ (show $ rows d) ++ "\n"

instance Show DisplayRow where
  show row = "\n" ++ (show $ cols row)

instance Show DisplayCol where
  show d = (show $ element d)

instance Show ElementType where
   show Wall  = "#"
   show Space = " "
   show Start = "S"
   show End   = "E"

instance Show Location where
  show loc = "(" ++ (show $ x loc) ++ "," ++ (show $ y loc) ++ ")"

instance Show Path where
  show path = (show $ from path) ++ "->" ++ (show $ to path)
  

initialiseMaze :: Globals -> Maze
initialiseMaze g = Maze { cells = [ defineCell g row col | row <- [1..width g], col <- [1..height g] ]
                        , paths = []
                        , stack = [begin g]
                        , visited = [begin g]
                        , counter = 1 }
  
generateDisplay :: Globals -> Maze -> Display
generateDisplay g m = Display [ getDisplayRow g m c | c <- [1..(1 + 2 * height g)] ]


getDisplayRow :: Globals -> Maze -> Int -> DisplayRow
getDisplayRow g m row = DisplayRow [ getDisplayCol g m row col | col <- [1..(1 + 2 * width g)] ]


getDisplayCol :: Globals -> Maze -> Int -> Int -> DisplayCol
getDisplayCol g m row col
  | isStart      = DisplayCol Start
  | isEnd        = DisplayCol End
  | isCell       = DisplayCol Space
  | isWall       = DisplayCol Wall
  | isEdge       = DisplayCol Wall
  | isPath       = DisplayCol Space
  | otherwise    = DisplayCol Wall
  where isStart  = (col `div` 2 == x (start g)) && (row `div` 2 == y (start g)) && isCell
        isEnd    = (col `div` 2 == x (end g)) && (row `div` 2 == y (end g)) && isCell
        isCell   = (col `mod` 2 == 0) && (row `mod` 2 == 0)
        isWall   = (col `mod` 2 == 1) && (row `mod` 2 == 1)
        isEdge   = (col == 1) || (row == 1) || (col == 1 + 2 * width g) || (row == 1 + 2 * height g)
        isPath   = checkPaths m col row
     
checkPaths :: Maze -> Int -> Int -> Bool
checkPaths m col row
  -- | trace ((show c) ++ " " ++ (show r)) False = undefined
  | (col `mod` 2 == 1) && (row `mod` 2 == 0)  = ( (Path left right) `elem` (paths m)) || ( (Path right left) `elem` (paths m))
  | (col `mod` 2 == 0) && (row `mod` 2 == 1)  = ( (Path up down) `elem` (paths m)) || ( (Path down up) `elem` (paths m))
  | otherwise                             = False
  where up        = Location (col `div` 2) ((row - 1) `div` 2)
        down      = Location (col `div` 2) ((row + 1) `div` 2)
        left      = Location ((col - 1) `div` 2) (row `div` 2)
        right     = Location ((col + 1) `div` 2) (row `div` 2)


unvisitedNeighbour :: Globals -> Maze -> Cell -> Location
unvisitedNeighbour g m c = (unvisited) !! ourSpecialInt
  where unvisited = [ z | z <- neighbours c, z `notElem` (visited m) ]
        ourSpecialInt = ((r g) !! (counter m)) `mod` (length unvisited)


generateMaze :: Globals -> Maze -> Maze
generateMaze g m
  | (stack m) == [] = error "Stack empty... This should never happen!" 
  | allCellsVisited = m
  | needToPopStack  = generateMaze g $ Maze { cells = cells m, paths = paths m, stack = tail $ stack m, visited = visited m, counter = counter m }
  | otherwise       = generateMaze g $ Maze { cells = cells m, paths = newPath : paths m, stack = newCellLocation : stack m, visited = newCellLocation : visited m, counter = (counter m)+1}
  where allCellsVisited = (length $ visited m) == (width g) * (height g)
        needToPopStack  = [ z | z <- neighbours currentCell, z `notElem` (visited m) ] == [] -- current cell's neighbours are all visited
        currentCell     = defineCell g (x topOfStack) (y topOfStack)
        newCellLocation = unvisitedNeighbour g m currentCell
        newPath         = Path topOfStack newCellLocation
        topOfStack      = head $ stack m

-- Define a cell
defineCell :: Globals -> Int -> Int -> Cell
defineCell g row col = Cell { location = Location row col, neighbours = getNeighbours g row col }

-- Get a list of the locations adjacent to a particular cell
getNeighbours :: Globals -> Int -> Int -> [Location]
getNeighbours g row col = [ location cell | cell <- candidateCells, isValidCell g cell ]
  where candidateCells = up ++ down ++ left ++ right
        up         = [defineCell g (row + 1) col]
        down       = [defineCell g (row - 1) col]
        left       = [defineCell g row (col - 1)]
        right      = [defineCell g row (col + 1)]


-- Test whether a candidate cell is in the maze
isValidCell :: Globals -> Cell -> Bool
isValidCell g c = ((location c) `elem` validLocations)
  where validLocations = [ Location row col | row <- [1..(width g)], col <- [1..(height g)] ]

main :: IO ()
main = do
  seed <- getStdGen
  let w = 5
  let h = 10
  let randomInts = randomRs (0,3) seed :: [Int]
  let startLocation = Location 1 1
  let endLocaton   = Location w h
  let beginLocation = endLocaton -- Location (width `div` 2) (height `div` 2)
  let g = Globals w h startLocation endLocaton beginLocation randomInts
  let initialMaze = initialiseMaze g
  let finalMaze = generateMaze g initialMaze
  -- putStrLn "\nHERE ARE THE PATHS:"
  -- putStrLn (show $ reverse $ paths finalMaze)
  
  let display = generateDisplay g finalMaze
  putStrLn (show display)