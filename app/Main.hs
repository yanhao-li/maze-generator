module Main where
import System.Exit(die);
import System.Environment (getArgs, getProgName)

import Maze

main :: IO ()
main = do
  args <- getArgs
  case args of
    [width, height] -> do
      print $ mazeGenerator (read width) (read height)
    _ -> do
      progName <- getProgName 
      die $ "Usage: " ++ progName ++ " <width> <height>"


