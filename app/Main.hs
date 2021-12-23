module Main where
import System.Exit(die);
import System.Environment (getArgs, getProgName)

import System.CPUTime
import Generator
import Control.DeepSeq
import System.Random (mkStdGen)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [width, height, seed] -> do
      -- start <- getCPUTime
      -- let r = mazeGenerator (read width) (read height) (mkStdGen (read seed))
      -- end <- r `deepseq` getCPUTime
      -- return (end - start)
      print $ mazeGenerator (read width) (read height) (mkStdGen (read seed))
    _ -> do
      progName <- getProgName 
      die $ "Usage: " ++ progName ++ " <width> <height>"


