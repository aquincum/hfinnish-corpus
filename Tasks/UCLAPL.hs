module Tasks.UCLAPL where

import Tasks.Task
import Hanalyze.ToUCLAP
import Hanalyze.Phoneme


task :: Task
task = Task
         doTask
         (Just FileName)
         "uclapl"
         "Create a UCLA Phonotactic Learner Training file and Features file based on the file passed to the program"

doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  convertFeaturesFile (addEdgeToInventory (theInventory flags) "#") "Features.txt"
  convertCorpusFile (theInventory flags) infn "Training.txt"
  --createNatClassFile (addEdgeToInventory (theInventory flags) "#") "NatClassesFile.txt"
