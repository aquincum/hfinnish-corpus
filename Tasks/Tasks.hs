{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tasks.Tasks where

--import Tasks.Options
import           Control.Monad
import           Data.Monoid
import           Hanalyze.Pattern
import           System.Console.GetOpt
import qualified Tasks.Options as Options

data Task = Task {
  runTask :: [Flag] -> IO (),
  taskFlagString :: String,
  descriptionString :: String
  }

instance Show Task where
  show t = "Task " ++ taskFlagString t
instance Eq Task where
  t1 == t2 = taskFlagString t1 == taskFlagString t2

-- |For my reflection trick to work!
instance Monoid Task where
  mempty = noTask
  mappend t1 t2 = Task
                    (\flags -> (runTask t1) flags >> (runTask) t2 flags)
                    (taskFlagString t1 ++ ", " ++ taskFlagString t2)
                    "Concatenation of tasks."

type IntParam = Int
instance Monoid IntParam where
  mempty = 1
  mappend = (+)

data Flag = TaskFlag Task
          | MaxN IntParam
          | SampleNone IntParam
          | SamplePatt IntParam
          | UCLAOutput Bool
          | FileName FilePath
          | FPattern [Pattern]
            deriving (Show, Eq)

tasks :: [Task]
tasks = [ noTask, helpTask ]


noTask :: Task
noTask = Task (\_ -> putStrLn "No task run.") "none" "This task does not do anything!"

helpTask :: Task
helpTask = Task (\_ -> putStrLn printUsage) "help" "This task only prints out the usage info of the program"

generateOptions :: [OptDescr Flag]
generateOptions =
  let
    nonTaskOptions = [Option ['n'] [] (ReqArg (MaxN . read) "n") "Maximum n of features in a bundle for the analyzeinventory task",
                      Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze for the analyzefile and the anderson task",
                      Option ['p'] ["pattern"] (ReqArg (FPattern . Options.readFPattern) "PATTERN") "Pattern to generate from. If task is generatefrompatt, it is required, but can be specified just plainly without -p",
                      Option ['u'] ["uclaoutput"] (NoArg (UCLAOutput True)) "Display help",
                      Option ['h'] ["help"] (NoArg (TaskFlag helpTask)) "Display help",
                      Option [] ["samplenone"] (ReqArg (SampleNone . read) "n") "Required for samplewugs: how many no-patterns to sample.",
                      Option [] ["samplepatt"] (ReqArg (SamplePatt . read) "n") "Required for samplewugs: how many sample patterns to sample."
                     ]
    taskStr = "which task to do."
    taskOption = Option ['t'] [] (ReqArg getTaskFlag "TASK") taskStr
  in
   taskOption:nonTaskOptions



-- |Ugly hack, can't Typeable yet.
--
-- >>> showConstructorName (MaxN 5)
-- "MaxN"
showConstructorName :: Show a => a -> String
showConstructorName = head . words . show

-- |Ugly hack too, but works.
--
-- >>> showConstructorNamePlain MaxN
-- "MaxN"
showConstructorNamePlain :: (Show a, Monoid a) => (a -> Flag) -> String
showConstructorNamePlain c = showConstructorName $ c mempty 

-- |Get any flag based on a constructor '(a -> Flag)', thanks
-- to my ugly 'showConstructorNamePlain' trick.
--
-- >>> getFlag flags MaxN
-- 5
getFlag :: (Show a, Monoid a) => [Flag] -> (a -> Flag) -> Maybe Flag
getFlag [] _ = Nothing
getFlag (f:fs) constr = if showConstructorName f == showConstructorNamePlain constr
                        then Just f
                        else getFlag fs constr


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




