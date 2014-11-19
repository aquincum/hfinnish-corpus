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

myForkIO :: IO () -> IO (MVar Int)
myForkIO io = do
  mvar <- newMVar 0 
  forkFinally io (\_ -> putMVar mvar 1)
  return mvar

main :: IO ()
main = do
  setNumCapabilities 3
  fns <- getArgs
  ncap <- getNumCapabilities
  putStrLn $ "Running with " ++ (show ncap) ++ " capabilities."
  let doAFile = (\file -> myForkIO (dealWithAFile file))
  mvars <- sequence $ map doAFile fns
  let sx = sequence $ map takeMVar mvars
  finished <- M.liftM (all (\x -> x==1)) sx
  sxnf <- sx
  let _ = force sxnf
  putStrLn $ "Wrapped up " ++ (if finished then "with all." else "with some.")
