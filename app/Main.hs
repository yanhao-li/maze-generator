module Main where
import System.Exit(die);
import System.Environment (getArgs, getProgName)

import Maze (Maze, mazeGenerator, printMaze)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [width, height] -> do
      printMaze $ mazeGenerator (read width) (read height)
    _ -> do
      progName <- getProgName 
      die $ "Usage: " ++ progName ++ " <width> <height>"


