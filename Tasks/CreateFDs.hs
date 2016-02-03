module Tasks.CreateFDs (task) where

import           Control.Concurrent
import           Control.Exception
import qualified Control.Monad as M
import           Data.List.Split (splitOn)
import           Hanalyze.FreqDist
import           Hanalyze.Process
import           Hanalyze.Progress
import           Tasks.Task

task :: Task
task = Task
  doTask
  (Just FileName)
  "createfds"
  "Creates freqdists from raw corpus. Runs on as many files as are given. Saves output to a file prefixed with freqdist_"
  

-- |Read one file, convert to FreqDists and output them to "freqdist_"++filename
dealWithAFile :: FilePath -> ProgVar -> IO ()
dealWithAFile fn progress = do
  let saveprefix = "freqdist_"
      pruned = last $ splitOn "/" fn
  --putStrLn ("Loading " ++ pruned)
  fd <- catch (readCountFreqs fn)
    (\e -> putStrLn ("Error in reading, " ++ show (e::SomeException)) >> return fdEmpty)
  --putStrLn ("File " ++ pruned ++ " read")
  saveTable fd (saveprefix ++ pruned)
  incrementProgVar progress
  progStr <-  printWithProgVal printProgress progress
  catch (putStrLn ("Done " ++ pruned ++ ". " ++ progStr))
    (\e -> putStrLn ("Error in printing, " ++ show (e::SomeException)))


doTask :: [Flag] -> IO ()
doTask flags = do
  let capabilities = case getFlag flags Capabilities of
        Nothing -> 3
        Just (Capabilities (IntParam i)) -> i
  setNumCapabilities capabilities
  let fns =  map (\(FileName f) -> f) $ getAllFlags flags FileName
  progress <- initializeProgVar fns
  ncap <- getNumCapabilities
  putStrLn $ "Running with " ++ show ncap ++ " capabilities."
  proto <- initProcess -- :: IO Process
  procs <- M.replicateM (length fns) (copyProcess proto)
  let procs' = map (startProcess . flip dealWithAFile progress) fns
  procs'' <- mapM (\pr -> copyProcess proto >>= pr) procs'
  procs''' <-  mapM waitProcess procs''
  return ()
