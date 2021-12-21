module Main where
import System.Exit(die);
import System.Environment (getArgs, getProgName)

import System.Random (getStdGen)
import System.CPUTime
import Generator
import Control.DeepSeq

main :: IO Integer
main = do
  args <- getArgs
  seed <- getStdGen
  case args of
    [width, height] -> do
      start <- getCPUTime
      let r = mazeGenerator (read width) (read height) seed
      end <- r `deepseq` getCPUTime
      return (end - start)

    _ -> do
      progName <- getProgName 
      die $ "Usage: " ++ progName ++ " <width> <height>"


