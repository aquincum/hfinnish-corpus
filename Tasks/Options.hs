module Tasks.Options where

import Hanalyze.Pattern
import System.Console.GetOpt
import Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import Control.Monad

-- |Flag for tasks to run with the executable
data TaskFlag = AnalyzeFile -- ^do the big analysis
          | AnalyzeInventory -- ^analyze the phonemic inventory: display relevant natural classes
          | Anderson -- ^Do the Anderson (1980) replication
          | GetLexStats -- ^Get the lexical statistics for the wug study
          | SplitFD -- ^Split a frequency distribution file to frontneutral and backneutral stems
          | SplitCut -- ^Do the 'SplitFD' task and cut the words until the last segment before the 2nd vowel
          | UCLAPL -- ^Produce input files for the UCLA Phonotactic Learner
          | Sublexical -- ^Produce input files for Becker's sublexical analyzer
          | Wugs -- ^Create wugs in CIC(C)A shape
          | SampleWugs -- ^Sample wugs from the UCLAPL output
          | GenerateExamplesForGrammar -- ^Generate examples for each constraint
            -- in an UCLAPL output grammar.txt
          | GenerateFromPatt -- ^Generate from dot pattern.
          | HarmSummary -- ^Summarize lex stats
          | Help -- ^Print out the help
          deriving (Show, Eq)

data Flag = TaskFlag TaskFlag
          | MaxN Int
          | SampleNone Int
          | SamplePatt Int
          | UCLAOutput Bool
          | FileName FilePath
          | FPattern [Pattern]
            deriving (Show, Eq)

readFPattern :: String -> [Pattern]
readFPattern s = case readPattern finnishInventory (T.pack s) of
  Nothing -> []
  Just p -> p

options :: [OptDescr Flag]
options = [
  Option ['t'] ["task"] (ReqArg optGetTaskFlag "task") "which task to do (analyzefile [default], analyzeinventory, anderson, getlexstats, split, splitcut, uclapl, sublexical, wugs, samplewugs, generateexamplesforgrammar, generatefrompatt, harmsummary)",
  Option ['n'] [] (ReqArg (MaxN . read) "n") "Maximum n of features in a bundle for the analyzeinventory task",
  Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze for the analyzefile and the anderson task",
  Option ['p'] ["pattern"] (ReqArg (FPattern . readFPattern) "PATTERN") "Pattern to generate from. If task is generatefrompatt, it is required, but can be specified just plainly without -p",
  Option ['i'] ["inventory"] (NoArg (TaskFlag AnalyzeInventory)) "Shortcut for -t analyzeinventory",
  Option ['u'] ["uclaoutput"] (NoArg (UCLAOutput True)) "Display help",
  Option ['h'] ["help"] (NoArg (TaskFlag Help)) "Display help",
  Option [] ["samplenone"] (ReqArg (SampleNone . read) "n") "Required for samplewugs: how many no-patterns to sample.",
  Option [] ["samplepatt"] (ReqArg (SamplePatt . read) "n") "Required for samplewugs: how many sample patterns to sample."
  ]

optGetTaskFlag :: String -> Flag
optGetTaskFlag s = case s of
  "analyzeinventory" -> TaskFlag AnalyzeInventory
  "anderson" -> TaskFlag Anderson
  "getlexstats" -> TaskFlag GetLexStats
  "split" -> TaskFlag SplitFD
  "splitcut" -> TaskFlag SplitCut
  "uclapl" -> TaskFlag UCLAPL
  "sublexical" -> TaskFlag Sublexical
  "wugs" -> TaskFlag Wugs
  "samplewugs" -> TaskFlag SampleWugs
  "generateexamplesforgrammar" -> TaskFlag GenerateExamplesForGrammar
  "generatefrompatt" -> TaskFlag GenerateFromPatt
  "harmsummary" -> TaskFlag HarmSummary
  "help" -> TaskFlag Help
  _ -> TaskFlag AnalyzeFile

compileOptions :: [String] -> IO ([Flag])
compileOptions args = case getOpt Permute options args of
  (o, n, []) -> do
    let hasMaxn  = any (\fl -> case fl of
                           MaxN _ -> True
                           _ -> False) o
        hasFn = any (\fl -> case fl of
                           FileName _ -> True
                           _ -> False) o
        hasPt = any (\fl -> case fl of
                        FPattern _ -> True
                        _ -> False) o
        hasTi = (TaskFlag AnalyzeInventory) `elem` o
        hasTf = (TaskFlag AnalyzeFile) `elem` o
        hasTa = (TaskFlag Anderson) `elem` o
        hasGl = (TaskFlag GetLexStats) `elem` o
        hasSp = (TaskFlag SplitFD) `elem` o
        hasSc = (TaskFlag SplitCut) `elem` o
        hasUC = (TaskFlag UCLAPL) `elem` o
        hasSl = (TaskFlag Sublexical) `elem` o
        hasWu = (TaskFlag Wugs) `elem` o
        hasSw = (TaskFlag SampleWugs) `elem` o
        hasGxfg = (TaskFlag GenerateExamplesForGrammar) `elem` o
        hasGfp = (TaskFlag GenerateFromPatt) `elem` o
        hasHarm = (TaskFlag HarmSummary) `elem` o
        hasHl = (TaskFlag Help) `elem` o
    when (hasHl) (error $ usageInfo "Usage: hanalyze1 [OPTIONS...] [FILE]" options)
    when (hasMaxn && hasFn) (myError ["both maxn and filename, can't deduce task"])
    when (hasMaxn && hasTa) (myError ["both maxn and anderson, ambiguous task"])
    when (hasFn && hasTi) (myError ["both filename and analyzeinventory, ambiguous task"])

    let retval | hasMaxn && not hasTi && not hasTf && not hasTa = TaskFlag AnalyzeFile:o
               | hasSp || hasSc || hasUC || hasSl || hasWu || hasSw || hasGl || hasGxfg || hasHarm = o
               | hasGfp && (UCLAOutput True) `elem` o = o
               | hasGfp && not ((UCLAOutput True) `elem` o) = UCLAOutput False:o
               | not hasMaxn && hasTi = MaxN 2:o
               | not hasMaxn && not hasTa && not hasTi && not hasTf = TaskFlag AnalyzeFile:MaxN 2:o
               | not hasMaxn && not hasTa && not hasTi && hasTf = MaxN 2:o
               | otherwise = o

        needsFile =    (TaskFlag AnalyzeFile) `elem` retval
                    || (TaskFlag Anderson) `elem` retval
                    || (TaskFlag GetLexStats) `elem` retval
                    || (TaskFlag SplitFD) `elem` retval
                    || (TaskFlag SplitCut) `elem` retval
                    || (TaskFlag UCLAPL) `elem` retval
                    || (TaskFlag Sublexical) `elem` retval
                    || (TaskFlag SampleWugs) `elem` retval
                    || (TaskFlag GenerateExamplesForGrammar) `elem` retval
                    || (TaskFlag HarmSummary) `elem` retval

        needsPattern = (TaskFlag GenerateFromPatt) `elem` retval
    
        retval' | needsFile && not hasFn && not (null n) = FileName (head n):retval
                | needsFile && not hasFn && null n = myError ["no FILE given either with -n or otherwise"]
                | needsPattern && not hasPt && not (null n) = FPattern (readFPattern $ head n):retval
                | needsPattern && not hasPt && null n = myError ["no PATTERN given either with -p or otherwise"]
                | otherwise = retval
    when (TaskFlag SampleWugs `elem` retval) $ do
      let hasNone = any (\fl -> case fl of
                          SampleNone _ -> True
                          _ -> False) retval'
      let hasPatt = any (\fl -> case fl of
                          SamplePatt _ -> True
                          _ -> False) retval'
      when (not hasNone && not hasPatt) (myError ["For a samplewugs task, give the number of samples to generate for no-patterns and patterns explicitly"])

    return retval'
  (_, _, errs) -> myError errs
 where
  myError errs = error $ "Option parsing error: " ++ concat errs ++
                  "\n" ++ usageInfo "Usage: hanalyze1 [OPTIONS...] [FILE]" options

flagGetMaxn :: [Flag] -> Int
flagGetMaxn [] = error "No maxn in flags"
flagGetMaxn (h:f) = case h of
  MaxN x -> x
  _ -> flagGetMaxn f

flagGetFn :: [Flag] -> String
flagGetFn [] = error "No filename in flags"
flagGetFn (h:f) = case h of
  FileName x -> x
  _ -> flagGetFn f

flagGetSampleNone :: [Flag] -> Int
flagGetSampleNone [] = error "No sample none in flags"
flagGetSampleNone (h:f) = case h of
  SampleNone x -> x
  _ -> flagGetSampleNone f

flagGetSamplePatt :: [Flag] -> Int
flagGetSamplePatt [] = error "No sample patt in flags"
flagGetSamplePatt (h:f) = case h of
  SamplePatt x -> x
  _ -> flagGetSamplePatt f

flagGetPattern :: [Flag] -> [Pattern]
flagGetPattern [] = error "No pattern in flags"
flagGetPattern (h:f) = case h of
  FPattern x -> x
  _ -> flagGetPattern f

flagGetUCLAOutput :: [Flag] -> Bool
flagGetUCLAOutput [] = error "No UCLA output flag in flags"
flagGetUCLAOutput (h:f) = case h of
  UCLAOutput x -> x
  _ -> flagGetUCLAOutput f

{- too complicated
flagGet :: [Flag] -> String -> Flag
flagGet flags s = case readConstr (dataTypeOf (MaxN 4)) s of
  Nothing -> error $ "No such flag as " ++ s
  Just f -> go flags f
 where
   go [] _ = error $ "No " ++ s ++ " in flags"
   go (h:f) c = if toConstr h == c
                then h
                else go f c
-}

