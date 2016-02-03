module Tasks.Sublexical (task) where

import Hanalyze.Phoneme
import Hanalyze.ToUCLAP
import Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "sublexical"
         "Create a training file for Michael Becker's sublexical learner."

doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  convertCorpusFileSublexical finnishInventory infn "sublex-training.txt"

