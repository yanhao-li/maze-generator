module Generator where

import qualified Maze as M
import qualified Wall as W
import Debug.Trace
import Data.Bifunctor
import Control.Monad
import Data.Array.IO
import Data.Set (Set, fromList, union, empty, difference)
import System.Random
import Control.Parallel.Strategies

mazeGenerator :: RandomGen g => Int -> Int -> Int -> g -> M.Maze
mazeGenerator deep width height seed =
  M.Maze {
    M.m = m,
    M.n = n,
    M.walls = initializeFrame m n `union` generateWallsPara deep 0 0 (m - 1) (n - 1) seed
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
generateWalls :: RandomGen g => Int -> Int -> Int -> Int -> g -> Set W.Wall 
generateWalls tr tc br bc g
  | bc - tc < 4 || br - tr < 4 = empty
  | otherwise = walls
    `union` topLeft 
    `union` topRight 
    `union` bottomLeft 
    `union` bottomRight
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    (g7, g8) = split g3
    (randomRow, _) = pickRandom [(tr + 2), (tr + 4)..(br - 2)] g1
    (randomCol, _) = pickRandom [(tc + 2), (tc + 4)..(bc - 2)] g2
    walls = verticalWalls `union` horizontalWalls `difference` holes
    verticalWalls = fromList [W.Wall {W.r = r, W.c = randomCol} | r <- [(tr + 1)..(br - 1)]]
    horizontalWalls = fromList [W.Wall {W.r = randomRow, W.c = c} | c <- [(tc + 1)..(bc - 1)]]
    topLeft = generateWalls tr tc randomRow randomCol g3
    topRight = generateWalls tr randomCol randomRow bc g4
    bottomLeft = generateWalls randomRow tc br randomCol g5
    bottomRight = generateWalls randomRow randomCol br bc g6
    (holes, _) = getHoles tr tc br bc randomRow randomCol g7

generateWallsPara 0 tr tc br bc g = generateWalls tr tc br bc g
generateWallsPara deep tr tc br bc g
  | bc - tc < 4 || br - tr < 4 = empty
  | otherwise = walls
    `union` runEval (rpar (topLeft `union` topRight))
    `union` runEval (rpar (bottomLeft `union` bottomRight))
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    (g7, g8) = split g3
    (randomRow, _) = pickRandom [(tr + 2), (tr + 4)..(br - 2)] g1
    (randomCol, _) = pickRandom [(tc + 2), (tc + 4)..(bc - 2)] g2
    walls = verticalWalls `union` horizontalWalls `difference` holes
    verticalWalls = fromList [W.Wall {W.r = r, W.c = randomCol} | r <- [(tr + 1)..(br - 1)]]
    horizontalWalls = fromList [W.Wall {W.r = randomRow, W.c = c} | c <- [(tc + 1)..(bc - 1)]]
    topLeft = generateWallsPara (deep - 1) tr tc randomRow randomCol g3
    topRight = generateWallsPara (deep - 1) tr randomCol randomRow bc g4
    bottomLeft = generateWallsPara (deep - 1) randomRow tc br randomCol g5
    bottomRight = generateWallsPara (deep - 1) randomRow randomCol br bc g6
    (holes, _) = getHoles tr tc br bc randomRow randomCol g7

getHoles :: RandomGen g => Int -> Int -> Int -> Int -> Int -> Int -> g -> (Set W.Wall, g)
getHoles tr tc br bc rr rc g = 
  (fromList $ map (choices !!) $ take 3 $ shuffle g [0..3], g5)
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    (g5, g6) = split g2
    randomPick l seed = l !! fst (randomR (0, length l - 1) g6)
    choices = [top, left, right, bottom] 
    (top, topSeed) = pickRandom [W.Wall {W.r = r, W.c = rc}| r <- [(tr + 1), (tr + 3)..(rr - 1)]] g1
    (left, leftSeed) = pickRandom [W.Wall {W.r = rr, W.c = c}| c <- [(tc + 1), (tc + 3)..(rc - 1)]] g2
    (right, rightSeed) = pickRandom [W.Wall {W.r = rr, W.c = c}| c <- [(rc + 1), (rc + 3)..(bc - 1)]] g3
    (bottom, bottomSeed) = pickRandom [W.Wall {W.r = r, W.c = rc}| r <- [(rr + 1), (rr + 3)..(br - 1)]] g4

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