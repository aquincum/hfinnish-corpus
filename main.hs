module Main where

import qualified Data.Map.Strict as Map
import System.IO
import System.Environment
import Data.List.Split (splitOn)
import Control.Applicative
import Control.Concurrent
import Hanalyze.FreqDist
import Hanalyze.Vowels

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]

-- |Read one file, convert to FreqDists and output them to "freqdist_"++filename
dealWithAFile :: FilePath -> IO ()
dealWithAFile fn = do
  let saveprefix = "freqdist_"
      pruned = last $ splitOn "/" fn
  putStrLn ("Loading " ++ pruned)
  fd <- readCountFreqs fn
  saveFreqDist fd (saveprefix ++ pruned)
  putStrLn ("Done " ++ pruned)


-- |old main: sequential reading of all files
sequentialMain :: [FilePath] -> IO ()
sequentialMain fns = do
  fd <- multiReadCountFreqs fns
  let filtered = FreqDist $ Map.filterWithKey (\s _ -> relevantStem (segment s) []) (getMap fd)
  saveFreqDist filtered "out.txt"
  
myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  forkFinally io (\_ -> putMVar mvar ())
  return mvar

main :: IO ()
main = do
--setNumCapabilities 3
  fns <- getArgs
  let doAFile = (\file -> myForkIO (dealWithAFile file))
  mvars <- sequence $ map doAFile fns
  let sx = map takeMVar mvars
  head sx
