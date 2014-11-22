module Hanalyze.Progress where

import Data.Time.Clock
import System.IO

-- |Type indicating progress
data Progress = Progress 
  Int -- ^total number of files in task
  UTCTime -- ^time at the start of the whole task
  Int -- ^processes already done
  
-- |Initializes a progress variable
initializeProgress :: [FilePath] -- the list of filenames to initialize nfiles from
                      -> IO Progress -- ^the new progress variable
initializeProgress fns = do
  let nfiles = length fns
  starttime <- getCurrentTime
  return $ Progress nfiles starttime 0

-- |Called when a process is done running.
incrementProgress :: Progress -> Progress
incrementProgress (Progress nf st pd) = Progress nf st (pd+1)

formatElapsed :: NominalDiffTime -> String
formatElapsed ndf
  | secs < 180 = (show $ floor secs) ++ "s"
  | secs < (60*60*3) = (show $ floor $ secs/60) ++ " minutes"
  | otherwise = (show $ floor $ secs/(60*60)) ++ " hours"
  where secs = toRational ndf

                           
-- |Returns a string representation of the progress.
printProgress :: Progress -> IO String
printProgress (Progress nf st pd) = do
  currtime <- getCurrentTime
  let diff = diffUTCTime currtime st
      nf' = fromIntegral nf
      pd' = fromIntegral pd
      perc = (pd'/nf')*100
      eta = if pd == 0 then 0 else diff * (nf'/pd' - 1)
  return $ (show pd) ++ "/" ++ (show nf) ++ "done, " ++ (show $ floor $ perc) ++
    "%. Time elapsed: " ++ (formatElapsed diff) ++ " seconds, ETA: " ++ (formatElapsed eta)
