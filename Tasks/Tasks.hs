{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tasks.Tasks where

import           Data.Maybe
import           System.Console.GetOpt
import qualified Tasks.AnalyzeFile
import qualified Tasks.AnalyzeInventory
import qualified Tasks.CreateFDs
import qualified Tasks.FilterFDs
import qualified Tasks.GenerateExamplesForGrammar
import qualified Tasks.GenerateFromPattern
import qualified Tasks.HarmSummary
import qualified Tasks.MergeFDs
import qualified Tasks.SampleWugs
import qualified Tasks.SplitByPercentiles
import qualified Tasks.SplitFrontBack
import qualified Tasks.Sublexical
import qualified Tasks.Summaries
import           Tasks.Task
import qualified Tasks.UCLAPL
import qualified Tasks.Wugs

-- |The available tasks for 'hanalyze', the executable.
tasks :: [Task]
tasks = [ noTask,
          helpTask,
          Tasks.AnalyzeFile.task,
          Tasks.AnalyzeInventory.task,
          Tasks.CreateFDs.task,
          Tasks.FilterFDs.taskStem,
          Tasks.FilterFDs.taskStemFilter,
          Tasks.FilterFDs.taskClean,
          Tasks.FilterFDs.taskClassicFilter,
          Tasks.GenerateExamplesForGrammar.task,
          Tasks.GenerateFromPattern.task,
          Tasks.HarmSummary.task,
          Tasks.MergeFDs.task,
          Tasks.SampleWugs.task,
          Tasks.SplitByPercentiles.task,
          Tasks.SplitFrontBack.taskfb,
          Tasks.SplitFrontBack.taskcut,
          Tasks.Sublexical.task,
          Tasks.Summaries.taskAnderson,
          Tasks.Summaries.taskLexStat,
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
    nonTaskOptions = [Option ['n'] [] (ReqArg (MaxN . readIntParam) "n") "Maximum n of features in a bundle for the analyzeinventory task / Number of bins in the splitbypercentiles task.",
                      Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze",
                      Option [] ["capabilities"] (ReqArg (Capabilities . readIntParam) "n") "The number of capabilities = CPU cores to use in tasks that use this feature.",
                      Option ['p'] ["pattern"] (ReqArg (FPattern . readFPattern) "PATTERN") "Pattern to generate from. If task is generatefrompatt, it is required, but can be specified just plainly without -p",
                      Option ['d'] ["alldiphthongs"] (NoArg (WithAllDiphthongs (BoolParam True))) "If present, all diphthongs will be used in the Finnish inventory, otherwise only /ei/ and /ie/",
                      Option ['u'] ["uclaoutput"] (NoArg (UCLAOutput (BoolParam True))) "If present, the output will be UCLAPL usable",
                      Option ['h'] ["help"] (NoArg (TaskFlag helpTask)) "Display help",
                      Option [] ["samplenone"] (ReqArg (SampleNone . readIntParam) "n") "Required for samplewugs: how many no-patterns to sample.",
                      Option [] ["samplepatt"] (ReqArg (SamplePatt . readIntParam) "n") "Required for samplewugs: how many sample patterns to sample.",
                      Option ['t'] ["filetype"] (ReqArg (FileType . readFFileType) "TYPE") "The type of the freqdist to read in. Default is a plain FreqDist."
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
             | taskflag o == Nothing -> return $ (taskInN n):(attachunnameds o (tail n) (taskInN n))
             | otherwise -> return $ attachunnameds o n (fromJust $ taskflag o)
  (_, _, errs) -> myerror errs
  where
    taskInN n = getTaskFlag (head n)
    attachunnameds :: [Flag] -> [String] -> Flag -> [Flag]
    attachunnameds fs [] _ = fs
    attachunnameds fs (h:t) tf = (toInsert h tf):(attachunnameds fs t tf)
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
    printTask t = taskFlagString t ++ ":\t" ++ descriptionString t ++ "\n-------------------\n"

-- |Getter for a task: returns 'TaskFlag' x where x is the task. If there is
-- no match, returns the 'helpTask'.
getTaskFlag :: String -> Flag
getTaskFlag s =
  let matched = filter (\t -> taskFlagString t == s) tasks
  in
   if length matched == 0 then TaskFlag helpTask
   else TaskFlag $ head matched

