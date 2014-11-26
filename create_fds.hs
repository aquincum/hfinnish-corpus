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
import qualified Data.Text.Lazy as T

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]

-- |Read one file, convert to FreqDists and output them to "freqdist_"++filename
dealWithAFile :: FilePath -> MVar Progress -> IO ()
dealWithAFile fn progress = do
  let saveprefix = "freqdist_"
      pruned = last $ splitOn "/" fn
  --putStrLn ("Loading " ++ pruned)
  fd <- readCountFreqs fn
  --putStrLn ("File " ++ pruned ++ " read")
  saveFreqDist fd (saveprefix ++ pruned)
  progVar <- takeMVar progress
  let progVarUpdated = incrementProgress progVar
  progStr <- printProgress progVarUpdated
  putMVar progress progVarUpdated
  catch (putStrLn ("Done " ++ pruned ++ ". " ++ progStr))
    (\e -> putStrLn ("Error in printing, " ++ (show (e::SomeException))))


main :: IO ()
main = do
  setNumCapabilities 3
  fns <- getArgs
  progVar <- initializeProgress fns
  progress <- newMVar progVar
  ncap <- getNumCapabilities
  putStrLn $ "Running with " ++ (show ncap) ++ " capabilities."
--  let pr = initProcess  >>= map startProcess (dealWithAFile $ fns)
--  pr >>= waitProcess
  proto <- initProcess -- :: IO Process
--  procs <- sequence $ map ((\x -> copyProcess proto) . startProcess . dealWithAFile) fns
--  threadDelay 1000
  procs <- sequence $ replicate (length fns) (copyProcess proto)
  -- let tasks = zip procs fns
  let procs' = map (\fn -> startProcess $ dealWithAFile fn progress) fns
  procs'' <- sequence $ map (\pr -> (copyProcess proto) >>= pr) procs'
  procs''' <-  sequence $ map waitProcess procs''
{-  finalPV <- takeMVar progress
  printProgress finalPV >>= putStrLn -}
  return ()
