module Maze where
import qualified Wall as W
import Data.Set (Set)
data Maze = Maze {
  m :: Int, -- number of matrix rows
  n :: Int, -- number of matrix cols
  walls :: Set W.Wall
}

instance Show Maze where
  show (Maze m' n' walls') =
    unlines maze
    where
      maze = [rowToStrng r| r <- [0 .. m'-1]]
      rowToStrng r = [if isWall r c then '#' else ' '| c <- [0 .. n'-1]]
      isWall r c = W.Wall r c `elem` walls'