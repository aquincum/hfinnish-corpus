{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.IO
import System.Environment
import qualified Data.Map as Map
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Pattern
import Hanalyze.Phoneme
import Hanalyze.Omorfi
import Control.Monad
import Data.Monoid
import Data.Maybe
-- import Data.Data
import qualified Hanalyze.Token as T
import System.Console.GetOpt

data Task = AnalyzeFile | AnalyzeInventory deriving (Show, Eq)
data Flag = Task Task
          | MaxN Int
          | FileName FilePath
            deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['t'] ["task"] (ReqArg optGetTask "task") "which task to do (analyzefile [default], analyzeinventory)",
  Option ['n'] [] (ReqArg (MaxN . read) "n") "Maximum n of features in a bundle for the analyzeinventory task",
  Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze for the analyzefile task"
  ]

optGetTask :: String -> Flag
optGetTask s = case s of
  "analyzeinventory" -> Task AnalyzeInventory
  _ -> Task AnalyzeFile

compileOptions :: [String] -> IO ([Flag])
compileOptions args = case getOpt Permute options args of
  (o, n, []) -> do
    let hasMaxn  = any (\fl -> case fl of
                           MaxN _ -> True
                           _ -> False) o
        hasFn = any (\fl -> case fl of
                           FileName _ -> True
                           _ -> False) o
        hasTi = (Task AnalyzeInventory) `elem` o
        hasTf = (Task AnalyzeFile) `elem` o
    when (hasMaxn && hasFn) (myError ["both maxn and filename, can't deduce task"])
    when (hasMaxn && hasTf) (myError ["both maxn and analyzefilename, ambiguous task"])
    when (hasFn && hasTi) (myError ["both filename and analyzeinventory, ambiguous task"])
    let retval = if hasMaxn
                 then if (not hasTi)
                      then Task AnalyzeInventory:o
                      else o
                 else if hasTi
                      then MaxN 2:o
                      else if (not hasTf)
                           then Task AnalyzeFile:o
                           else o
    let retval' = if (Task AnalyzeFile) `elem` retval
                  then if (not hasFn)
                       then if (not $ null n)
                            then FileName (head n):retval
                            else myError ["no FILE given either with -n or otherwise"]
                       else retval
                  else retval
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


sectionHeader :: String -> IO ()
sectionHeader s = putStrLn s >> putStrLn "========"

dataPointInt :: String -> Int -> IO ()
dataPointInt label val = putStrLn $ label ++ ": " ++ show val

summarySection :: FreqDist -> IO ()
summarySection fd = sectionHeader "Summary" >>
                    dataPointInt "grand total" (sumTable fd)


vowelSummarySection :: (Show x, Eq x) => String -> FreqDist -> (Token -> x) -> IO ()
vowelSummarySection str fd f =
  sectionHeader ("Vowel structure summary -- " ++ str)  >>
  writeTable summedfd stdout >>
  putStrLn ""
  where
    summedfd = summarizeFD f fd

summarizeByC :: FreqDist -> IO ()
summarizeByC fd = do
  let fdLab = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF labial, DotF $ mconcat [low,vowel]]) fd
  let fdCor = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF coronal, DotF $ mconcat [low,vowel]]) fd
  let fdVel = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF velar, DotF $ mconcat [low,vowel]]) fd
  vowelSummarySection "with labials" fdLab harmonicity
  vowelSummarySection "with coronals" fdCor harmonicity
  vowelSummarySection "with velars" fdVel harmonicity

summarizeAnderson :: FreqDist -> IO AnnotatedFreqDist
summarizeAnderson fd = do
  let l p = fromJust $ findPhoneme finnishInventory p
      gravesNotP = [l "k", l "g", l "kk", l "m", l "mm", l "ng", l "f", l "ff", l "v", l "vv", l "h", l "hh", l "j", l "jj"]
      acutesP = [l "p", l "pp", l "t", l "tt", l "d", l "dd", l "n", l "nn", l "s", l "ss", l "z", l "zz", l "š", l "šš", l "ž", l "žž", l "l", l "ll", l "r", l "rr"]
      funGrave  = filterToken finnishInventory [DotF consonant, DotF vowel, AnyP gravesNotP, DotF $ mconcat [low,vowel]]
      funAcuteI = filterToken finnishInventory [DotF consonant, AnyP [l "i", l "ii", l "ei"], AnyP acutesP, DotF $ mconcat [low,vowel]]
      funAcuteE = filterToken finnishInventory [DotF consonant, AnyP [l "e", l "ee"], AnyP acutesP, DotF $ mconcat [low,vowel]]

      -- todo: QuestionF
      
      fdGrave = filterTable funGrave fd
      fdAcuteI = filterTable funAcuteI fd
      fdAcuteE = filterTable funAcuteE fd
  vowelSummarySection "with graves without [p] (Anderson: disharmonic)" fdGrave harmonicity
  vowelSummarySection "with acutes or [p] after [i(:),ei] (Anderson: disharmonic)" fdAcuteI harmonicity
  vowelSummarySection "with acutes or [p] after [e(:)] (Anderson: harmonic)" fdAcuteE harmonicity
  return $ annotateFD [("grave", funGrave), ("acute with i", funAcuteI), ("acute with e", funAcuteE)] fd

filterVowelFinals :: FreqDist -> FreqDist
filterVowelFinals = filterTable $ filterToken finnishInventory [Star, DotF vowel]

main :: IO ()
main = do
  args <- getArgs
  flags <- compileOptions args
  when ((Task AnalyzeInventory) `elem` flags) $ do
    -- something different
    let relb = selectRelevantBundles finnishInventory (flagGetMaxn flags)
        phonemes = map (pickByFeature finnishInventory) relb
        phnames = (map . map) phonemeName phonemes
        outputzip = zip relb phnames
    mapM_ (\zline -> putStr (show $ fst zline) >>
                     putStr ": " >>
                     mapM_ (\ph -> putStr (ph ++ " ")) (snd zline) >>
                     putStrLn "") outputzip
  when ((Task AnalyzeFile) `elem` flags) $ do
    --  fd <- liftM filterVowelFinals $ readFreqDist $ head args    fd <- readFreqDist $ head args
    fd <- readFreqDist $ flagGetFn flags
    summarySection fd
    vowelSummarySection "plain vowel structure" fd onlyVowels
    vowelSummarySection "plain harmonicity" fd harmonicity
    putStrLn "# MAIN FD"
    withFile "summary_annot_fd.txt" WriteMode (\h -> do
                                                  annotfd <- summarizeAnderson fd
                                                  writeTable annotfd h
                                              )
    putStrLn "# ONLY >10 FREQ FD"
    summarizeAnderson $ filterByValTable (> 10) fd
    putStrLn "# ONLY >50 FREQ FD"
    summarizeAnderson $ filterByValTable (> 50) fd
    putStrLn "# ONLY >100 FREQ FD"
    summarizeAnderson $ filterByValTable (> 100) fd
    --  saveTable fd' a"test.out"
    return ()
