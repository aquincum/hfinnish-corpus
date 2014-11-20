module Main where

import qualified Data.Map.Strict as Map
import System.IO
import System.Environment
import Data.List.Split (splitOn)
import qualified Control.Monad as M
import Control.Concurrent
import Control.DeepSeq
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Process

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]

-- |Read one file, convert to FreqDists and output them to "freqdist_"++filename
dealWithAFile :: FilePath -> IO ()
dealWithAFile fn = do
  let saveprefix = "freqdist_"
      pruned = last $ splitOn "/" fn
  putStrLn ("Loading " ++ pruned)
  fd <- readCountFreqs fn
  putStrLn ("File " ++ pruned ++ " read")
  saveFreqDist fd (saveprefix ++ pruned)
  putStrLn ("Done " ++ pruned)


-- |old main: sequential reading of all files
sequentialMain :: [FilePath] -> IO ()
sequentialMain fns = do
  fd <- multiReadCountFreqs fns
  let filtered = FreqDist $ Map.filterWithKey (\s _ -> relevantStem (segment s) []) (getMap fd)
  saveFreqDist filtered "out.txt"

main :: IO ()
main = do
  setNumCapabilities 3
  fns <- getArgs
  ncap <- getNumCapabilities
  let pr = initProcess >>= startProcess (dealWithAFile $ head fns)
  pr >>= waitProcess
  putStrLn $ "Running with " ++ (show ncap) ++ " capabilities."
