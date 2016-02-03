{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tasks.Tasks where

import           Data.Monoid
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import           System.Console.GetOpt
import           System.Exit
import           Tasks.Task
import qualified Tasks.UCLAPL

tasks :: [Task]
tasks = [ noTask,
          helpTask,
          Tasks.UCLAPL.task
        ]


helpTask :: Task
helpTask = Task (\_ -> putStrLn printUsage) "help" "This task only prints out the usage info of the program"


readFPattern :: String -> [Pattern]
readFPattern s = case readPattern finnishInventory (T.pack s) of
  Nothing -> []
  Just p -> p


generateOptions :: [OptDescr Flag]
generateOptions =
  let
    nonTaskOptions = [Option ['n'] [] (ReqArg (MaxN . read) "n") "Maximum n of features in a bundle for the analyzeinventory task",
                      Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze for the analyzefile and the anderson task",
                      Option ['p'] ["pattern"] (ReqArg (FPattern . readFPattern) "PATTERN") "Pattern to generate from. If task is generatefrompatt, it is required, but can be specified just plainly without -p",
                      Option ['u'] ["uclaoutput"] (NoArg (UCLAOutput True)) "Display help",
                      Option ['h'] ["help"] (NoArg (TaskFlag helpTask)) "Display help",
                      Option [] ["samplenone"] (ReqArg (SampleNone . read) "n") "Required for samplewugs: how many no-patterns to sample.",
                      Option [] ["samplepatt"] (ReqArg (SamplePatt . read) "n") "Required for samplewugs: how many sample patterns to sample."
                     ]
    taskStr = "which task to do."
    taskOption = Option ['t'] [] (ReqArg getTaskFlag "TASK") taskStr
  in
   taskOption:nonTaskOptions





compileOptions :: [String] -> IO ([Flag])
compileOptions args = case getOpt Permute generateOptions args of
  (o, n, []) | length n == 0 && (taskflag o) == Nothing -> myerror ["No task specified!"]
             | length n == 0 -> return o
             | taskflag o == Nothing -> return $ (getTaskFlag (head n)):(attachfiles o (tail n))
             | otherwise -> return $ attachfiles o n
  (_, _, errs) -> myerror errs
  where
    attachfiles :: [Flag] -> [String] -> [Flag]
    attachfiles fs [] = fs
    attachfiles fs (h:t) = FileName h:(attachfiles fs t)
    taskflag o = getFlag o (TaskFlag)
    myerror errs = error $ "Option parsing error: " ++ concat errs ++
      "\n--------------------------------------\n\n" ++ printUsage

printUsage :: String
printUsage = usageInfo "Usage: hanalyze [TASK] [OPTIONS...] [FILE]" generateOptions ++ "\n\nAvailable tasks:\n" ++ concatMap printTask tasks
  where
    printTask t = taskFlagString t ++ ":\t" ++ descriptionString t ++ "\n"


getTaskFlag :: String -> Flag
getTaskFlag s =
  let matched = filter (\t -> taskFlagString t == s) tasks
  in
   if length matched == 0 then TaskFlag helpTask
   else TaskFlag $ head matched

