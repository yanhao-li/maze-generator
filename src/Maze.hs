module Maze
( Maze
, mazeGenerator
, printMaze
) where

type Maze = [[Bool]]

mazeGenerator :: Int -> Int -> Maze
mazeGenerator width height = replicate height $ replicate width False

printMaze :: Maze -> IO ()
printMaze maze = mapM_ putStrLn $ map (map (\x -> if x then '#' else '.')) maze