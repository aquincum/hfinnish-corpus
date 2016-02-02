{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import Data.List (intersperse)
import Text.Printf
import Data.Random
import Data.Random.Source.DevRandom
-- import Data.Data
import qualified Hanalyze.Token as T
import System.Console.GetOpt
import Hanalyze.ToUCLAP
import qualified Data.Text as Txt
import qualified Data.Text.IO as TIO
import Control.Monad.Writer



-- |Flag for tasks to run with the executable
data Task = AnalyzeFile -- ^do the big analysis
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
          | DisyllSummary -- ^Summarize lex stats
          | Help -- ^Print out the help
          deriving (Show, Eq)

data Flag = Task Task
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
  Option ['t'] ["task"] (ReqArg optGetTask "task") "which task to do (analyzefile [default], analyzeinventory, anderson, getlexstats, split, splitcut, uclapl, sublexical, wugs, samplewugs, generateexamplesforgrammar, generatefrompatt, disyllsummary)",
  Option ['n'] [] (ReqArg (MaxN . read) "n") "Maximum n of features in a bundle for the analyzeinventory task",
  Option ['f'] ["file"] (ReqArg FileName "FILE") "The file to analyze for the analyzefile and the anderson task",
  Option ['p'] ["pattern"] (ReqArg (FPattern . readFPattern) "PATTERN") "Pattern to generate from. If task is generatefrompatt, it is required, but can be specified just plainly without -p",
  Option ['i'] ["inventory"] (NoArg (Task AnalyzeInventory)) "Shortcut for -t analyzeinventory",
  Option ['u'] ["uclaoutput"] (NoArg (UCLAOutput True)) "Display help",
  Option ['h'] ["help"] (NoArg (Task Help)) "Display help",
  Option [] ["samplenone"] (ReqArg (SampleNone . read) "n") "Required for samplewugs: how many no-patterns to sample.",
  Option [] ["samplepatt"] (ReqArg (SamplePatt . read) "n") "Required for samplewugs: how many sample patterns to sample."
  ]

optGetTask :: String -> Flag
optGetTask s = case s of
  "analyzeinventory" -> Task AnalyzeInventory
  "anderson" -> Task Anderson
  "getlexstats" -> Task GetLexStats
  "split" -> Task SplitFD
  "splitcut" -> Task SplitCut
  "uclapl" -> Task UCLAPL
  "sublexical" -> Task Sublexical
  "wugs" -> Task Wugs
  "samplewugs" -> Task SampleWugs
  "generateexamplesforgrammar" -> Task GenerateExamplesForGrammar
  "generatefrompatt" -> Task GenerateFromPatt
  "disyllsummary" -> Task DisyllSummary
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
        hasPt = any (\fl -> case fl of
                        FPattern _ -> True
                        _ -> False) o
        hasTi = (Task AnalyzeInventory) `elem` o
        hasTf = (Task AnalyzeFile) `elem` o
        hasTa = (Task Anderson) `elem` o
        hasGl = (Task GetLexStats) `elem` o
        hasSp = (Task SplitFD) `elem` o
        hasSc = (Task SplitCut) `elem` o
        hasUC = (Task UCLAPL) `elem` o
        hasSl = (Task Sublexical) `elem` o
        hasWu = (Task Wugs) `elem` o
        hasSw = (Task SampleWugs) `elem` o
        hasGxfg = (Task GenerateExamplesForGrammar) `elem` o
        hasGfp = (Task GenerateFromPatt) `elem` o
        hasDis = (Task DisyllSummary) `elem` o
        hasHl = (Task Help) `elem` o
    when (hasHl) (error $ usageInfo "Usage: hanalyze1 [OPTIONS...] [FILE]" options)
    when (hasMaxn && hasFn) (myError ["both maxn and filename, can't deduce task"])
    when (hasMaxn && hasTa) (myError ["both maxn and anderson, ambiguous task"])
    when (hasFn && hasTi) (myError ["both filename and analyzeinventory, ambiguous task"])

    let retval | hasMaxn && not hasTi && not hasTf && not hasTa = Task AnalyzeFile:o
               | hasSp || hasSc || hasUC || hasSl || hasWu || hasSw || hasGl || hasGxfg || hasDis = o
               | hasGfp && (UCLAOutput True) `elem` o = o
               | hasGfp && not ((UCLAOutput True) `elem` o) = UCLAOutput False:o
               | not hasMaxn && hasTi = MaxN 2:o
               | not hasMaxn && not hasTa && not hasTi && not hasTf = Task AnalyzeFile:MaxN 2:o
               | not hasMaxn && not hasTa && not hasTi && hasTf = MaxN 2:o
               | otherwise = o

        needsFile =    (Task AnalyzeFile) `elem` retval
                    || (Task Anderson) `elem` retval
                    || (Task GetLexStats) `elem` retval
                    || (Task SplitFD) `elem` retval
                    || (Task SplitCut) `elem` retval
                    || (Task UCLAPL) `elem` retval
                    || (Task Sublexical) `elem` retval
                    || (Task SampleWugs) `elem` retval
                    || (Task GenerateExamplesForGrammar) `elem` retval
                    || (Task DisyllSummary) `elem` retval

        needsPattern = (Task GenerateFromPatt) `elem` retval
    
        retval' | needsFile && not hasFn && not (null n) = FileName (head n):retval
                | needsFile && not hasFn && null n = myError ["no FILE given either with -n or otherwise"]
                | needsPattern && not hasPt && not (null n) = FPattern (readFPattern $ head n):retval
                | needsPattern && not hasPt && null n = myError ["no PATTERN given either with -p or otherwise"]
                | otherwise = retval
    when (Task SampleWugs `elem` retval) $ do
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


sectionHeader :: String -> IO ()
sectionHeader s = putStrLn s >> putStrLn "========"

dataPointInt :: String -> Int -> IO ()
dataPointInt label val = putStrLn $ label ++ ": " ++ show val

summarySection :: FreqDist -> IO ()
summarySection fd = sectionHeader "Summary" >>
                    dataPointInt "grand total" (sumTable fd)


data VowelSummaryInfo = VowelSummaryInfo { getTokenFreq :: Double,
                                           getTypeFreq :: Double,
                                           getTokenPerc :: Double,
                                           getTypePerc :: Double
                                         } deriving (Eq, Show)
-- |Resulting from 'vowelSummarySection'
data VowelSummaryFreqDist = VowelSummaryFreqDist {getVSFDMap :: Map.Map Token VowelSummaryInfo} deriving (Eq, Show)
instance Table VowelSummaryFreqDist VowelSummaryInfo where
  tEmpty = VowelSummaryFreqDist Map.empty
  tConstruct = const VowelSummaryFreqDist
  tGetMap = getVSFDMap
  tPrintfun _ (mkey, mval) = mconcat [mkey, "\t",
                                      T.pack $ printf "%d\t" ((round $ getTokenFreq mval) :: Int),
                                      T.pack $ printf "%d\t" ((round $ getTypeFreq mval) :: Int),
                                      T.pack $ printf "%.2f%%\t" ((getTokenPerc mval) :: Double),
                                      T.pack $ printf "%.2f%%\t" ((getTypePerc mval) :: Double)
                                      ]

vowelSummarySection :: (Show x, Eq x) => String -> FreqDist -> (Token -> x) -> IO VowelSummaryFreqDist
vowelSummarySection str fd f =
  sectionHeader ("Vowel structure summary -- " ++ str)  >>
  writeout >>
  putStrLn "" >>
  return tokenperc
  where
    summedMap = Map.toList $ tGetMap $ summarizeFD f fd -- (Int, Int) ~ (token, type)
    allToken = fromIntegral $ foldr ((+) . fst . snd) 0 summedMap
    allType = fromIntegral $  foldr ((+) . snd . snd) 0 summedMap
    tokenperc = tFromList $ map (\(t, (a,b)) -> (t, VowelSummaryInfo (fromIntegral a) (fromIntegral b) ((fromIntegral a) / allToken * 100.0) ((fromIntegral b) / allType * 100.0))) summedMap
    writeout = mapM putStrLn (map (T.unpack . (tPrintfun tokenperc)) (tToList tokenperc))
--saveVowelSummary :: Map.Map Token [Double]


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
      acutesP = [l "p", l "pp", l "t", l "tt", l "d", l "dd", l "n", l "nn", l "s", l "ss", l "z", l "zz", l "l", l "ll", l "r", l "rr"]
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
  let getTypeFreqs m = map (getTypeFreq . snd) (tToList m)
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


fitsPattern :: [Pattern] -> Token -> Bool
fitsPattern patt tkn =
  let
    phons = segment finnishInventory tkn
  in
   case phons of
     Just phs -> filterWord phs patt
     Nothing -> False

annotateWithPatterns :: FreqDist -> AnnotatedFreqDist
annotateWithPatterns fd =
  let patternAnnotMap :: [(Annotation, Token -> Bool)]
      patternAnnotMap = [
        (T.pack "1", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.j{-consonantal}.*"))),
        (T.pack "2", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.{-consonantal}.*"))),
        (T.pack "3", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal,+high}.[p,pp]{-consonantal}.*"))),
        (T.pack "4", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.[l,ll]{-consonantal}.*"))),
        (T.pack "5", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.[f,s,h,v]j{-consonantal}.*")))
        ]
  in
   annotateFD patternAnnotMap fd
                    


-- |Keep only stems that have a pair: ending in -a ~ ending in -ä
findCouples :: FreqDist -> FreqDist
findCouples fd =
  let
    revfd = mapTable T.reverse fd
    getHead t = case T.uncons t of
      Just (c,_) -> c
      Nothing -> 'x'
    isMyPairInThere pairc pair t = case T.uncons t of
      Just (c,left) -> (T.cons pairc left) `elem` (fdKeys pair)
      Nothing -> False
    as = filterTable (\t -> getHead t == 'a') revfd
    aes = filterTable (\t -> getHead t == 'ä') revfd
    asinaestoo = filterTable (isMyPairInThere 'ä' aes) as
    aesinastoo = filterTable (isMyPairInThere 'a' as) aes
    remerged = asinaestoo <> aesinastoo
  in
   mapTable T.reverse remerged

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
    --createNatClassFile finnishInventoryWithEdges "NatClassesFile.txt"

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

  when ((Task GenerateFromPatt) `elem` flags) $ do
    let patt = flagGetPattern flags
    when (length patt == 0) (error "Illegal pattern.")
    when (not (isDotPattern patt)) (error "Not a dot pattern!")
    let phs = case generatePattern finnishInventory patt of
          Nothing -> []
          Just phons -> phons
        words = map spellout phs
    when (not (flagGetUCLAOutput flags)) $ mapM_ (T.hPutStrLn stdout) words
    when (flagGetUCLAOutput flags) $ do
      let fd = tFromList (zip words (replicate (length words) 1))
          (corp, probs) = runWriter $ convertCorpus finnishInventory fd
      TIO.putStrLn corp
      when (not (probs == Txt.empty)) (putStrLn "Problems:\n" >> TIO.putStrLn probs)

  when ((Task SampleWugs) `elem` flags) $ do
    let infn = flagGetFn flags
    contents <- TIO.readFile infn
    putStrLn "File read."
    let cWithAe = Txt.intercalate (Txt.pack "ä") (Txt.splitOn (Txt.pack "ae") contents)
        cWithOe = Txt.intercalate (Txt.pack "ö") (Txt.splitOn (Txt.pack "oe") cWithAe)
        allWugs = readUCLAPLOutput cWithOe infn
    putStrLn $ "Umlauts reitroduced. n = " ++ show (tSize allWugs)
    let zeroWeight = filterByValTable (== 0.0) allWugs
        zeroWeightFD = (tFromList $ map (\(t,_) -> (t,0)) (tToList zeroWeight)) :: FreqDist
    putStrLn $ "Zero weights filtered. n = " ++ show (tSize zeroWeightFD)
    let noSpacesFD = mapTable (T.filter (/= ' ')) zeroWeightFD
    putStrLn $ "Spaces removed. n = " ++ show (tSize noSpacesFD)
    morphanalyzed <- analyseFDOmorfi noSpacesFD
    putStrLn $ "Morphological parsing done. n = " ++ show (tSize morphanalyzed)
    let unknownsFD = takeStems $ getUnknownWords morphanalyzed
    putStrLn $ "Known words filtered. n = " ++ show (tSize unknownsFD)
    let unknownPairsFD = findCouples unknownsFD
    putStrLn $ "Unknown pairs found. n = " ++ show (tSize unknownPairsFD)
    let annotated = annotateWithPatterns unknownPairsFD 
    putStrLn $ "Annotation done. Sampling..."
    let createSample :: ([Flag] -> Int) -> String -> IO AnnotatedFreqDist
        createSample fromFlags annot =
          let
            part = tToList $ filterWithAnnotation (==T.pack annot) annotated
          in
            runRVar (shuffleNofM (min (fromFlags flags) (length part)) (length part) part) DevURandom >>=
            return . tFromList
    nonesSample <- createSample flagGetSampleNone ""
    pattSample <- mapM (createSample flagGetSamplePatt) ["1","2","3","4","5"]
    putStrLn $ "Sampling done."
    withFile "sampled-wugs.txt" WriteMode $ \h ->
      writeTable nonesSample h >>
      mapM_ (flip writeTable h) pattSample
  
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
  when ((Task DisyllSummary) `elem` flags) $ do
    fd <- readFreqDist $ flagGetFn flags
    let disyllPattern = fromJust $ readPattern finnishInventory "{+consonantal}*{-consonantal}.{+consonantal}*{-consonantal}.{+consonantal}*"
        disylls = filterTableByPattern disyllPattern fd
        summary = summarizeFD (abbreviateBFNs . wordHarmonies) disylls
    writeTable summary stdout
  when ((Task GetLexStats) `elem` flags) $ do
    fd <- readFreqDist $ flagGetFn flags -- you want filtered3_all here, the whole corpus
    let pattern = fromJust $ readPattern finnishInventory "{+consonantal}*[i,ii,ie,e,ei,ee]{+consonantal}*[a,ä,aa,ää]{+consonantal}*"
        vfinals = filterTableByPattern pattern fd
        allCorpusAAePattern = fromJust $ readPattern finnishInventory "*[a,ä,aa,ää]{+consonantal}*"
        allCorpusAAeDisyllPattern = fromJust $ readPattern finnishInventory "{+consonantal}*{-consonantal}.{+consonantal}*[a,ä,aa,ää]{+consonantal}*"
        allCorpusAAe = filterTableByPattern allCorpusAAePattern fd
        allCorpusAAeDisyll = filterTableByPattern allCorpusAAeDisyllPattern fd
    withFile "vowelfinals.txt" WriteMode (writeTable vfinals)
    summarySection vfinals
    vowelSummarySection "plain vowel structure" vfinals onlyVowels
    vowelSummarySection "plain harmonicity" vfinals harmonicity
    lv <- vowelSummarySection "last vowel" vfinals lastVowel
    allCorpusLv <- vowelSummarySection "last vowel" allCorpusAAe lastVowel
    allCorpusDisyllLv <- vowelSummarySection "last vowel" allCorpusAAeDisyll lastVowel
    withFile "wordfinal.txt" WriteMode $ \h ->
      hPutStrLn h "In all corpus:" >>
      writeTable allCorpusLv h >>
      hPutStrLn h "\nIn all corpus disyllabic:" >>
      writeTable allCorpusDisyllLv h >>
      hPutStrLn h "\nOnly neutral disyllabic:" >>
      writeTable lv h
    sltable <- vowelSummarySection "stem vowel & last vowel" vfinals stemLastVowel
    withFile "lexstats.txt" WriteMode (writeTable sltable)
    let patternannotated = annotateWithPatterns vfinals
        printPatternAfd :: AnnotatedFreqDist -> String -> Handle -> IO ()
        printPatternAfd afd annot h = do
          hPutStrLn h ""
          hPutStrLn h $ "=== PATTERN " ++ annot ++ " ==="
          let fd = dropAnnotation $ filterWithAnnotation (== T.pack annot) afd
          psltable <- vowelSummarySection ("stem vowel & last vowel with pattern " ++ annot) fd stemLastVowel
          writeTable psltable h
    withFile "lexstats-pattern.txt" WriteMode (\h ->
                                                mapM (\a -> printPatternAfd patternannotated a h) ["","1","2","3","4","5"])
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
  when ((Task GenerateExamplesForGrammar) `elem` flags) $ do
    txt <- TIO.readFile $ flagGetFn flags
    let uclagr = readUCLAPLGrammar txt (flagGetFn flags)
    let patts = map generateExamples uclagr
        pattsStr = map (\p -> mconcat $ intersperse (T.pack ",") p) patts
    mapM_ (\(uc, wds) ->
            putStrLn (show uc ++ "\t" ++ (T.unpack wds))) (zip uclagr pattsStr)
        
    -- explore
    
lastVowel :: Token -> String
lastVowel t = case segment finnishInventory t of
  Nothing -> "segment-error"
  Just x -> go x
    where
      isVowel ph = isPhoneme ph vowel
      go phs
        | phs == [] = "none"
        | isVowel (last phs) = phonemeName (last phs)
        | otherwise =  go (init phs)

stemLastVowel :: Token -> [String]
stemLastVowel t = case segment finnishInventory t of
  Nothing -> []
  Just phs -> map phonemeName $ filter (flip isPhoneme vowel) phs
    
