module Tasks.HarmSummary (task) where

import Hanalyze.FreqDist
import Hanalyze.Phoneme
import Hanalyze.ToUCLAP
import Hanalyze.Vowels
import System.IO
import Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "harmsummary"
         "Creates a simple summary of the different harmonicities of the words in the passed freqdist in a form of B's N's and F's."

doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  convertCorpusFileSublexical finnishInventory infn "sublex-training.txt"
  fd <- readFreqDist infn
  let doSegment wd = case segment finnishInventory wd of
        Nothing -> []
        Just s -> s
      summary = summarizeFD (abbreviateBFNs . wordHarmonies . doSegment) fd
  writeTable summary stdout
