{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tasks.Tasks where

import           Data.Maybe
import           System.Console.GetOpt
import qualified Tasks.GenerateFromPattern
import qualified Tasks.Sublexical
import           Tasks.Task
import qualified Tasks.UCLAPL
import qualified Tasks.Wugs

-- |The available tasks for 'hanalyze', the executable.
tasks :: [Task]
tasks = [ noTask,
          helpTask,
          Tasks.GenerateFromPattern.task,
          Tasks.Sublexical.task,
          Tasks.UCLAPL.task,
          Tasks.Wugs.task
        ]

-- |A task that just prints out usage.
helpTask :: Task
helpTask = Task (\_ -> putStrLn printUsage) Nothing "help" "This task only prints out the usage info of the program"


-- |The options list needed by GetOpt
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




-- |Compiles the arguments to flags
compileOptions :: [String] -> IO ([Flag])
compileOptions args = case getOpt Permute generateOptions args of
  (o, n, []) | length n == 0 && (taskflag o) == Nothing -> myerror ["No task specified!"]
             | length n == 0 -> return o
             | taskflag o == Nothing -> return $ (taskInN n):(attachfiles o (tail n) (taskInN n))
             | otherwise -> return $ attachfiles o n (fromJust $ taskflag o)
  (_, _, errs) -> myerror errs
  where
    taskInN n = getTaskFlag (head n)
    attachfiles :: [Flag] -> [String] -> Flag -> [Flag]
    attachfiles fs [] _ = fs
    attachfiles fs (h:t) tf = (toInsert h tf):(attachfiles fs t tf)
    toInsert :: String -> Flag -> Flag
    toInsert s (TaskFlag t) = case parseUnflaggedOption t of
      Nothing -> FlagNoop
      Just f -> f s
    taskflag o = getFlag o (TaskFlag)
    myerror errs = error $ "Option parsing error: " ++ concat errs ++
      "\n--------------------------------------\n\n" ++ printUsage

-- |The usage screen
printUsage :: String
printUsage = usageInfo "Usage: hanalyze [TASK] [OPTIONS...] [FILE]" generateOptions ++ "\n\nAvailable tasks:\n" ++ concatMap printTask tasks
  where
    printTask t = taskFlagString t ++ ":\t" ++ descriptionString t ++ "\n\n"

-- |Getter for a task: returns 'TaskFlag' x where x is the task. If there is
-- no match, returns the 'helpTask'.
getTaskFlag :: String -> Flag
getTaskFlag s =
  let matched = filter (\t -> taskFlagString t == s) tasks
  in
   if length matched == 0 then TaskFlag helpTask
   else TaskFlag $ head matched

