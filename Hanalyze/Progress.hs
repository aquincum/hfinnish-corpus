-- |Implements some functions to take care of progress within the program
module Hanalyze.Progress where

import Data.Time.Clock
import Control.Monad
import System.IO

-- |Type indicating progress. Fields are:
--
-- 1. An 'Int' with the total number of objects in task
--
-- 2. A 'UTCTime' that is the time at the start of the whole task
--
-- 3. An 'Int' with the number of processes already done
--
data Progress = Progress Int UTCTime Int

-- |Initializes a progress variable with a filename list
initializeProgress :: [a] -- ^the list of objects to initialize nobjs from
                      -> IO Progress -- ^the new progress variable
initializeProgress fns = do
  let nobjs = length fns
  starttime <- getCurrentTime
  return $ Progress nobjs starttime 0

-- |Called when a process is done running.
incrementProgress :: Progress -> Progress
incrementProgress (Progress nf st pd) = Progress nf st (pd+1)

-- |Formats the time elapsed in a 'Progress'
formatElapsed :: NominalDiffTime -> String
formatElapsed ndf
  | secs < 180 = show (floor secs) ++ "s"
  | secs < (60*60*3) = show (floor $ secs/60) ++ " minutes"
  | otherwise = show (floor $ secs/(60*60)) ++ " hours"
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
  return $ show pd ++ "/" ++ show nf ++ "done, " ++ show (floor perc) ++
    "%. Time elapsed: " ++ formatElapsed diff ++ ", ETA: " ++ formatElapsed eta

-- |Print only every n'th progress report, as well as 0% and 100%
printEveryNth :: Progress -- ^The progress to print out
                 -> Int -- ^n
                 -> IO ()
printEveryNth prog@(Progress nf st pd) n =
  when (pd == 0 || pd == nf || pd `mod` n == 0) (printProgress prog >>= putStrLn)

-- |Print only at every % increase
printEveryPercent :: Progress -> IO ()
printEveryPercent prog@(Progress nf st pd) =
  let nfpercent = nf `div` 100 in
  printEveryNth prog nfpercent
  
