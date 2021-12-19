module Main where
import System.Exit(die);
import System.Environment (getArgs, getProgName)

import System.Random (getStdGen)
import Generator

main :: IO ()
main = do
  args <- getArgs
  seed <- getStdGen
  case args of
    [width, height] -> do
      print $ mazeGenerator (read width) (read height) seed
    _ -> do
      progName <- getProgName 
      die $ "Usage: " ++ progName ++ " <width> <height>"


