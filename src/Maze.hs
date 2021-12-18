module Maze where

data Wall = Wall {
  r :: Int,
  c :: Int
} deriving (Show, Eq)

data Maze = Maze {
  m :: Int, -- number of matrix rows
  n :: Int, -- number of matrix cols
  walls :: [Wall]
}

instance Show Maze where
  show (Maze m n walls) =
    unlines maze
    where
      maze = [rowToStrng r| r <- [0..m-1]]
      rowToStrng r = [if isWall r c then '#' else ' '| c <- [0..n-1]]
      isWall r c = Wall r c `elem` walls

mazeGenerator :: Int -> Int -> Maze
mazeGenerator width height =
  let maze = initializeMaze width height
  in
    maze

initializeMaze :: Int -> Int -> Maze
initializeMaze width height =
  Maze {
    m = m,
    n = n,
    walls = walls
  }
  where
    walls = [ Wall {r = r, c = c} | r <- [0..m - 1], 
                                    c <- [0..n - 1],
                                    r == 0 || r == m - 1 || c == 0 || c == n - 1 ]
    m = width * 2 + 1 -- number of matrix rows
    n = height * 2 + 1 -- number of matrix cols