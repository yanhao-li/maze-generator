module Generator where

import qualified Maze as M
import qualified Wall as W
import System.Random

mazeGenerator :: RandomGen g => Int -> Int -> g -> M.Maze
mazeGenerator width height seed =
  M.Maze {
    M.m = m,
    M.n = n,
    M.walls = initializeFrame width height
  }
  where
    m = width * 2 + 1 -- number of matrix rows
    n = height * 2 + 1 -- number of matrix cols

initializeFrame :: Int -> Int -> [W.Wall]
initializeFrame m n = [ W.Wall {W.r = r, W.c = c} | r <- [0..m - 1], 
                                            c <- [0..n - 1],
                                            r == 0 || r == m - 1 || c == 0 || c == n - 1 ]

-- Given an frame, generate walls inside the frame
generateWalls :: RandomGen g => Int -> Int -> Int -> Int -> g -> [W.Wall]
generateWalls tx ty bx by seed
  | width < 2 || height < 2 = []
  | width < height = tl ++ lr ++ bl ++ br
  | otherwise = tl ++ lr ++ bl ++ br
  where
    width = (bx - tx) `div` 2
    height = (by - ty) `div` 2
    tl = generateWalls tx ty (tx + width) (ty + height) seed
    lr = generateWalls (tx + width) ty bx (ty + height) seed
    bl = generateWalls tx (ty + height) (tx + width) by seed
    br = generateWalls (tx + width) (ty + height) bx by seed