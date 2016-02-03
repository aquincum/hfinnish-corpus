module Tasks.Wugs (task) where

import Hanalyze.Phoneme
import Hanalyze.ToUCLAP
import Hanalyze.Token as T
import System.IO
import Tasks.Task

task :: Task
task = Task
         doTask
         Nothing
         "wugs"
         "Creates wugs in the CIC(C)A shape needed for the second chapter. Outputs to stdout."

doTask :: [Flag] -> IO ()
doTask _ = do
  let wugs1 = generateCICAWugs1
  let wugs2 = generateCICAWugsCluster
  let wugs3 = generateCICAWugsHiatus
  mapM_ (T.hPutStrLn stdout . spellout) wugs1 
  mapM_ (T.hPutStrLn stdout . spellout) wugs2 
  mapM_ (T.hPutStrLn stdout . spellout) wugs3

