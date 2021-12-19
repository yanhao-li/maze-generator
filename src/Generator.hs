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
  | width < 2 || height < 2 = empty
  | width < height = generateHWalls tr tc br bc seed
  | otherwise = generateVWalls tr tc br bc seed
  where
    width = (bc - tc) `div` 2
    height = (br - tr) `div` 2

-- Generate horizontal walls to divide the space
generateHWalls :: RandomGen g => Int -> Int -> Int -> Int -> g -> Set W.Wall
generateHWalls tr tc br bc seed
  | trace ("generateHWalls for the space: " ++ show (tr, tc, br, bc)) False = undefined
  | br - tr < 4 || bc - tc < 4 = empty
  | otherwise = fromList [W.Wall {W.r = randomRow, W.c = c} | c <- [tc + 1, bc - 1] ] `union` top `union` bottom
  where 
    (randomRow, newSeed) = randomR (tr + 2, br - 2) seed
    top = generateWalls tr tc randomRow bc newSeed
    bottom = generateWalls tr randomRow br bc newSeed

-- Generate vertically walls divide the space
generateVWalls :: RandomGen g => Int -> Int -> Int -> Int -> g -> Set W.Wall
generateVWalls tr tc br bc seed
  | trace ("generateVWalls for the space: " ++ show (tr, tc, br, bc)) False = undefined
  | br - tr < 4 || bc - tc < 4 = empty
  | otherwise = fromList [W.Wall {W.r = r, W.c = randomCol} | r <- [tc + 1, bc - 1] ] `union` left `union` right
  where 
    (randomCol, newSeed) = randomR (tr + 2, br - 2) seed
    left = generateWalls tr tc br randomCol newSeed
    right = generateWalls randomCol tc br bc newSeed