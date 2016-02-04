module Tasks.HarmSummary (task) where

import Control.Monad
import Data.Monoid
import Hanalyze.FreqDist
import Hanalyze.Phoneme
import Hanalyze.Vowels
import System.IO
import Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "harmsummary"
         "Creates a simple summary of the different harmonicities of the words in the passed freqdist in a form of B's N's and F's. Now with multiple files!"

doTask :: [Flag] -> IO ()
doTask flags = do
  let fns = map (\(FileName f) -> f) $ getAllFlags flags FileName
  when (length fns < 1) $ error "No files given." 
  summTables <- mapM doOneFile fns
  let bigSummary = mconcat summTables
  writeTable bigSummary stdout

  
doOneFile :: FilePath -> IO SummaryTable
doOneFile fp = do
  fd <- readFreqDist fp
  let doSegment wd = case segment finnishInventory wd of
        Nothing -> []
        Just s -> s
      summary = summarizeFD (abbreviateBFNs . wordHarmonies . doSegment) fd
  return summary
