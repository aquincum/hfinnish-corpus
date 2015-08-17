{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Main where

import System.IO
import Control.Exception
import System.Environment
import qualified Data.Map as Map
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Pattern
import Hanalyze.Phoneme
import Hanalyze.Omorfi
import Hanalyze.Chisq
import Control.Monad
import Data.Monoid
import Data.Maybe
import Text.Printf
-- import Data.Data
import qualified Hanalyze.Token as T
import System.Console.GetOpt
import Hanalyze.ToUCLAP




-- |Flag for tasks to run with the executable
data Task = AnalyzeFile -- ^do the big analysis
          | AnalyzeInventory -- ^analyze the phonemic inventory: display relevant natural classes
          | Anderson -- ^Do the Anderson (1980) replication
          | SplitFD -- ^Split a frequency distribution file to frontneutral and backneutral stems
          | SplitCut -- ^Do the 'SplitFD' task and cut the words until the last segment before the 2nd vowel
          | UCLAPL -- ^Produce input files for the UCLA Phonotactic Learner
          | Sublexical -- ^Produce input files for Becker's sublexical analyzer
          | Wugs -- ^Create wugs in CIC(C)A shape
          | Help -- ^Print out the help
          deriving (Show, Eq)

data Flag = Task Task
          | MaxN Int
          | FileName FilePath
            deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['t'] ["task"] (ReqArg optGetTask "task") "which task to do (analyzefile [default], analyzeinventory, anderson, split, splitcut, uclapl, sublexical, wugs)",
  Option ['n'] [] (ReqArg (MaxN . read) "n") "Maximum n of features in a bundle for the analyzeinventory task",
  Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze for the analyzefile and the anderson task",
  Option ['i'] ["inventory"] (NoArg (Task AnalyzeInventory)) "Shortcut for -t analyzeinventory",
  Option ['h'] ["help"] (NoArg (Task Help)) "Display help"
  ]

optGetTask :: String -> Flag
optGetTask s = case s of
  "analyzeinventory" -> Task AnalyzeInventory
  "anderson" -> Task Anderson
  "split" -> Task SplitFD
  "splitcut" -> Task SplitCut
  "uclapl" -> Task UCLAPL
  "sublexical" -> Task Sublexical
  "wugs" -> Task Wugs
  "help" -> Task Help
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
        hasTa = (Task Anderson) `elem` o
        hasSp = (Task SplitFD) `elem` o
        hasSc = (Task SplitCut) `elem` o
        hasUC = (Task UCLAPL) `elem` o
        hasSl = (Task Sublexical) `elem` o
        hasWu = (Task Wugs) `elem` o
        hasHl = (Task Help) `elem` o
    when (hasHl) (error $ usageInfo "Usage: hanalyze1 [OPTIONS...] [FILE]" options)
    when (hasMaxn && hasFn) (myError ["both maxn and filename, can't deduce task"])
    when (hasMaxn && hasTa) (myError ["both maxn and anderson, ambiguous task"])
    when (hasFn && hasTi) (myError ["both filename and analyzeinventory, ambiguous task"])

    let retval | hasMaxn && not hasTi && not hasTf && not hasTa = Task AnalyzeFile:o
               | hasSp || hasSc || hasUC || hasSl || hasWu = o
               | not hasMaxn && hasTi = MaxN 2:o
               | not hasMaxn && not hasTa && not hasTi && not hasTf = Task AnalyzeFile:MaxN 2:o
               | not hasMaxn && not hasTa && not hasTi && hasTf = MaxN 2:o
               | otherwise = o

        needsFile =    (Task AnalyzeFile) `elem` retval
                    || (Task Anderson) `elem` retval
                    || (Task SplitFD) `elem` retval
                    || (Task SplitCut) `elem` retval
                    || (Task UCLAPL) `elem` retval
                    || (Task Sublexical) `elem` retval
    
        retval' | needsFile && not hasFn && not (null n) = FileName (head n):retval
                | needsFile && not hasFn && null n = myError ["no FILE given either with -n or otherwise"]
                | otherwise = retval

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


vowelSummarySection :: (Show x, Eq x) => String -> FreqDist -> (Token -> x) -> IO (Map.Map Token [Double])
vowelSummarySection str fd f =
  sectionHeader ("Vowel structure summary -- " ++ str)  >>
  writeout >>
  putStrLn "" >>
  return (Map.fromList tokenperc)
  where
    summedMap = Map.toList $ tGetMap $ summarizeFD f fd -- (Int, Int) ~ (token, type)
    allToken = fromIntegral $ foldr ((+) . fst . snd) 0 summedMap
    allType = fromIntegral $  foldr ((+) . snd . snd) 0 summedMap
    tokenperc = map (\(t, (a,b)) -> (t, [(fromIntegral a),(fromIntegral b),(fromIntegral a) / allToken * 100.0, (fromIntegral b) / allType * 100.0])) summedMap
    writeout = mapM (\(t, list) -> putStr (T.unpack t) >>
                                   putStr "\t" >>
                                   mapM (\x -> printf "%d\t" ((round x)::Int)) (take 2 list) >>
                                   mapM (\x -> printf "%.2f%%\t" (x::Double)) (drop 2 list) >>
                                   putStr "\n") tokenperc

summarizeByC :: FreqDist -> IO ()
summarizeByC fd = do
  let fdLab = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF labial, DotF $ mconcat [low,vowel]]) fd
  let fdCor = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF coronal, DotF $ mconcat [low,vowel]]) fd
  let fdVel = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF velar, DotF $ mconcat [low,vowel]]) fd
  vowelSummarySection "with labials" fdLab harmonicity
  vowelSummarySection "with coronals" fdCor harmonicity
  vowelSummarySection "with velars" fdVel harmonicity
  return ()

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


summarizeByPattern :: FreqDist -> PhonemicInventory -> [Pattern] -> IO AnnotatedFreqDist
summarizeByPattern fd inv patt = do
  let funFits = filterToken inv patt
      funDoesntfit = not . funFits
      fdFits = filterTable funFits fd
      fdDoesntfit = filterTable funDoesntfit fd
  putStrLn $ " ========*** " ++ writePattern patt ++ " ***========"
  fitMap       <- vowelSummarySection ("fitting pattern " ++ writePattern patt) fdFits harmonicity
  doesntfitMap <- vowelSummarySection ("not fitting pattern " ++ writePattern patt) fdDoesntfit harmonicity
  let getTypeFreqs m = map ((!! 1) .snd) (Map.toList m)
      table = [getTypeFreqs fitMap, getTypeFreqs doesntfitMap]
  putStrLn $ show table
  catch (do
            let xsqtest = runChiSqTest table True
            putStrLn $ "\nChi Sq = " ++ show (chisq xsqtest) ++ ", p = " ++ show (p xsqtest) ++ ", " ++ if sig xsqtest then "*SIGNIFICANT*" else "n.s."
        ) (\err -> putStrLn $"No chi square test available, " ++ (show (err::SomeException)))
  return $ annotateFD [(T.pack ("fits_"++writePattern patt), funFits),
                       (T.pack ("fitsnot_"++writePattern patt), funDoesntfit)] fd


filterVowelFinals :: FreqDist -> FreqDist
filterVowelFinals = filterTable $ filterToken finnishInventory [Star, DotF vowel]

main :: IO ()
main = do
  args <- getArgs
  flags <- compileOptions args

  when ((Task SplitFD) `elem` flags || (Task SplitCut) `elem` flags) $ do 
    fd <- readFreqDist $ flagGetFn flags
    let front = filterTable (\t -> harmonicity t == FrontNeutral) fd
        back = filterTable (\t -> harmonicity t == BackNeutral) fd
    when ((Task SplitCut) `elem` flags) $ do
      let patt = case readPattern finnishInventory "{+consonantal}*{-consonantal}.{-consonantal}*{+consonantal}*" of
            Nothing -> error "readpattern"
            Just p -> p
      let takeUntil2ndV tok = case segment finnishInventory tok of
            Nothing -> T.pack ""
            Just seg -> case matchWord seg patt of
              Nothing -> T.pack "no match"
              Just matched -> spellout matched
      let front' = tMap takeUntil2ndV front 
      let back' = tMap takeUntil2ndV back
      saveTable front' (flagGetFn flags ++ "_front_cut")
      saveTable back' (flagGetFn flags ++ "_back_cut")
    when ((Task SplitFD) `elem` flags) $ do
      saveTable front (flagGetFn flags ++ "_front")
      saveTable back (flagGetFn flags ++ "_back")

  when ((Task UCLAPL) `elem` flags) $ do
    let infn = flagGetFn flags
    convertFeaturesFile finnishInventoryWithEdges "Features.txt"
    convertCorpusFile finnishInventory infn "Training.txt"
    createNatClassFile finnishInventoryWithEdges "NatClassesFile.txt"

  when ((Task Sublexical) `elem` flags) $ do
    let infn = flagGetFn flags
    --    createNatClassFile finnishInventoryWithEdges "NatClassesFile.txt"
    convertCorpusFileSublexical finnishInventory infn "sublex-training.txt"


  when ((Task Wugs) `elem` flags) $ do
    let wugs1 = generateCICAWugs1
    let wugs2 = generateCICAWugsCluster
    let wugs3 = generateCICAWugsHiatus
    withFile "wugs.txt" WriteMode (\h ->
                                    mapM_ (T.hPutStrLn h . spellout) wugs1 >>
                                    mapM_ (T.hPutStrLn h . spellout) wugs2 >>
                                    mapM_ (T.hPutStrLn h . spellout) wugs3
                                    )


  
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
  when ((Task Anderson) `elem` flags) $ do
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
  when ((Task AnalyzeFile) `elem` flags) $ do
    fd <- readFreqDist $ flagGetFn flags
    summarySection fd
    -- get relevant bundles for vowels
    let vowels = filterInventoryByBundle finnishInventory vowel
        vowelRelevants = selectRelevantBundles vowels (flagGetMaxn flags)
        fd' = filterTable (filterToken finnishInventory [StarF consonant, DotF vowel, StarF consonant, StarF vowel]) fd
        patternGenerator fb = [StarF consonant, DotF fb, StarF consonant, StarF vowel]
        patterns = map patternGenerator vowelRelevants
    mapM_ (summarizeByPattern fd' finnishInventory) patterns
        
    -- explore
    
