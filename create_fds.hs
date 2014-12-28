module Main where

import qualified Data.Map.Strict as Map
import System.IO
import System.Environment
import Data.List.Split (splitOn)
import qualified Control.Monad as M
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Process
import Hanalyze.Progress

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]

-- |Read one file, convert to FreqDists and output them to "freqdist_"++filename
dealWithAFile :: FilePath -> MVar Progress -> IO ()
dealWithAFile fn progress = do
  let saveprefix = "freqdist_"
      pruned = last $ splitOn "/" fn
  --putStrLn ("Loading " ++ pruned)
  fd <- readCountFreqs fn
  --putStrLn ("File " ++ pruned ++ " read")
  saveTable fd (saveprefix ++ pruned)
  progVar <- takeMVar progress
  let progVarUpdated = incrementProgress progVar
  progStr <- printProgress progVarUpdated
  putMVar progress progVarUpdated
  catch (putStrLn ("Done " ++ pruned ++ ". " ++ progStr))
    (\e -> putStrLn ("Error in printing, " ++ show (e::SomeException)))


main :: IO ()
main = do
  setNumCapabilities 3
  fns <- getArgs
  progVar <- initializeProgress fns
  progress <- newMVar progVar
  ncap <- getNumCapabilities
  putStrLn $ "Running with " ++ show ncap ++ " capabilities."
  proto <- initProcess -- :: IO Process
  procs <- M.replicateM (length fns) (copyProcess proto)
  let procs' = map (startProcess . flip dealWithAFile progress) fns
  procs'' <- mapM (\pr -> copyProcess proto >>= pr) procs'
  procs''' <-  mapM waitProcess procs''
  _ <- takeMVar progress
  return ()
