module Generator where

import qualified Maze as M
import qualified Wall as W
import Debug.Trace
import Data.Set (Set, fromList, union, empty)
import System.Random

mazeGenerator :: RandomGen g => Int -> Int -> g -> M.Maze
mazeGenerator width height seed =
  M.Maze {
    M.m = m,
    M.n = n,
    M.walls = initializeFrame m n `union` generateWalls 0 0 (m - 1) (n - 1) seed
    -- M.walls = initializeFrame m n
  }
  where
    m = width * 2 + 1 -- number of matrix rows
    n = height * 2 + 1 -- number of matrix cols

initializeFrame :: Int -> Int -> Set W.Wall
initializeFrame m n = fromList [ W.Wall {W.r = r, W.c = c} | r <- [0..m - 1], 
                                            c <- [0..n - 1],
                                            r == 0 || r == m - 1 || c == 0 || c == n - 1 ]

-- Given an frame, generate walls inside the frame
-- tr: the row number of top left wall
-- tc: the col number of top left wall
-- br: the row number of bottom right wall
-- bc: the col number of bottom right wall
generateWalls :: RandomGen g => Int -> Int -> Int -> Int -> g -> Set W.Wall
generateWalls tr tc br bc seed
  -- | trace ("generateHWalls for the space: " ++ show (tx, ty, bx, by)) False = undefined
  | bc - tc < 4 || br - tr < 4 = empty
  | otherwise = verticalWalls `union` horizontalWalls `union` topLeft `union` topRight `union` bottomLeft `union` bottomRight
  where
    (randomRow, newRowSeed) = randomR (tr + 2, br - 2) seed
    (randomCol, newColSeed) = randomR (tc + 2, bc - 2) seed
    (verticalHole1, newHoleSeed) = randomR (tr + 1, randomRow - 1) seed
    (verticalHole2, newHoleSeed2) = randomR (randomRow + 1, br - 1) newHoleSeed
    (horizontalHole, _) = randomR (tc + 1, randomCol - 1) newHoleSeed2
    verticalWalls = fromList [W.Wall {W.r = r, W.c = randomCol} | r <- [(tr + 1)..(br - 1)], r /= verticalHole1, r /= verticalHole2]
    horizontalWalls = fromList [W.Wall {W.r = randomRow, W.c = c} | c <- [(tc + 1)..(bc - 1)], c /= horizontalHole]
    topLeft = generateWalls tr tc randomRow randomCol newRowSeed
    topRight = generateWalls tr randomCol randomRow bc newColSeed
    bottomLeft = generateWalls randomRow tc br randomCol newRowSeed
    bottomRight = generateWalls randomRow randomCol br bc newColSeed