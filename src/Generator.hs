module Generator where

import qualified Maze as M
import qualified Wall as W
import Debug.Trace
import Data.Bifunctor
import Control.Monad
import Data.Array.IO
import Data.Set (Set, fromList, union, empty, difference)
import System.Random

mazeGenerator :: RandomGen g => Int -> Int -> g -> M.Maze
mazeGenerator width height seed =
  M.Maze {
    M.m = m,
    M.n = n,
    M.walls = initializeFrame m n `union` fst (generateWalls 0 0 (m - 1) (n - 1) seed)
    -- M.walls = initializeFrame m n
  }
  where
    m = width * 2 + 1 -- number of matrix rows
    n = height * 2 + 1 -- number of matrix cols

initializeFrame :: Int -> Int -> Set W.Wall
initializeFrame m n = fromList frame
  where 
    frame = [ W.Wall {W.r = r, W.c = c} |
      r <- [0..m - 1], 
      c <- [0..n - 1],
      r == 0 || r == m - 1 || c == 0 || c == n - 1,
      (r, c) /= (0, 1), -- entrace
      (r, c) /= (m - 1, n - 2)] -- exit

-- Given an frame, generate walls inside the frame
-- top left wall cell: (tr, tc)
-- bottom right wall cell: (br, bc)
generateWalls :: RandomGen g => Int -> Int -> Int -> Int -> g -> (Set W.Wall, g)
generateWalls tr tc br bc seed
  -- | trace ("generateHWalls for the space: " ++ show (tx, ty, bx, by)) False = undefined
  -- | trace ("holes: " ++ show holes) False = undefined
  | bc - tc < 4 || br - tr < 4 = (empty, seed)
  | otherwise = (walls
    `union` topLeft 
    `union` topRight 
    `union` bottomLeft 
    `union` bottomRight, newSeed5)
  where
    (randomRow, newRowSeed) = pickRandom [(tr + 2), (tr + 4)..(br - 2)] seed
    (randomCol, newColSeed) = pickRandom [(tc + 2), (tc + 4)..(bc - 2)] newRowSeed
    walls = verticalWalls `union` horizontalWalls `difference` holes
    verticalWalls = fromList [W.Wall {W.r = r, W.c = randomCol} | r <- [(tr + 1)..(br - 1)]]
    horizontalWalls = fromList [W.Wall {W.r = randomRow, W.c = c} | c <- [(tc + 1)..(bc - 1)]]
    (topLeft, newSeed1) = generateWalls tr tc randomRow randomCol newColSeed
    (topRight, newSeed2) = generateWalls tr randomCol randomRow bc newSeed1
    (bottomLeft, newSeed3) = generateWalls randomRow tc br randomCol newSeed2
    (bottomRight, newSeed4) = generateWalls randomRow randomCol br bc newSeed3
    (holes, newSeed5) = getHoles tr tc br bc randomRow randomCol newSeed4

getHoles :: RandomGen g => Int -> Int -> Int -> Int -> Int -> Int -> g -> (Set W.Wall, g)
getHoles tr tc br bc rr rc seed = 
  (fromList $ map (choices !!) $ take 3 $ shuffle seed [0..3], bottomSeed)
  where
    randomPick l seed = l !! fst (randomR (0, length l - 1) seed)
    choices = [top, left, right, bottom] 
    (top, topSeed) = pickRandom [W.Wall {W.r = r, W.c = rc}| r <- [(tr + 1), (tr + 3)..(rr - 1)]] seed
    (left, leftSeed) = pickRandom [W.Wall {W.r = rr, W.c = c}| c <- [(tc + 1), (tc + 3)..(rc - 1)]] topSeed
    (right, rightSeed) = pickRandom [W.Wall {W.r = rr, W.c = c}| c <- [(rc + 1), (rc + 3)..(bc - 1)]] leftSeed
    (bottom, bottomSeed) = pickRandom [W.Wall {W.r = r, W.c = rc}| r <- [(rr + 1), (rr + 3)..(br - 1)]] rightSeed

pickRandom :: RandomGen g => [a] -> g -> (a, g)
pickRandom l seed = Data.Bifunctor.first (l !!) (randomR (0, length l - 1) seed)

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen [] = [] 
shuffle gen list = randomElem : shuffle newGen newList
  where 
   randomTuple = randomR (0, length list - 1) gen
   randomIndex = fst randomTuple
   newGen      = snd randomTuple
   randomElem  = list !! randomIndex
   newList     = take randomIndex list ++ drop (randomIndex+1) list