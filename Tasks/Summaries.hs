{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tasks.Summaries where

import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Hanalyze.FreqDist
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import           Hanalyze.Vowels
import           System.IO
import           Tasks.Task
import           Text.Printf

taskAnderson :: Task
taskAnderson = Task
         doTaskAnderson
         (Just FileName)
         "anderson"
         "Analyzes Anderson (1980)'s predictions on a corpus of CIC(C)A stems."

taskLexStat :: Task
taskLexStat = Task
         doTaskLexStat
         (Just FileName)
         "getlexstats"
         "Runs various lexical statistics. 2015 version."


doTaskAnderson :: [Flag] -> IO ()
doTaskAnderson flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  fd <- readFreqDist infn
  summarySection fd
  vowelSummarySection "plain vowel structure" fd onlyVowels
  vowelSummarySection "plain harmonicity" fd harmonicity
  putStrLn "# MAIN FD"
  withFile "summary_annot_fd.txt" WriteMode (\h -> do
                                                annotfd <- summarizeAnderson fd flags
                                                writeTable annotfd h
                                            )
  putStrLn "# ONLY >10 FREQ FD"
  summarizeAnderson (filterByValTable (> 10) fd) flags
  putStrLn "# ONLY >50 FREQ FD"
  summarizeAnderson (filterByValTable (> 50) fd) flags
  putStrLn "# ONLY >100 FREQ FD"
  summarizeAnderson (filterByValTable (> 100) fd) flags
  --  saveTable fd' a"test.out"
  return ()

doTaskLexStat :: [Flag] -> IO ()
doTaskLexStat flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  fd <- readFreqDist infn -- you want filtered3_all here, the whole corpus
  let pattern = fromJust $ readPattern (theInventory flags) "{+consonantal}*[i,ii,ie,e,ei,ee]{+consonantal}*[a,ä,aa,ää]{+consonantal}*"
      vfinals = filterTableByPattern pattern fd
      allCorpusAAePattern = fromJust $ readPattern (theInventory flags) "*[a,ä,aa,ää]{+consonantal}*"
      allCorpusAAeDisyllPattern = fromJust $ readPattern (theInventory flags) "{+consonantal}*{-consonantal}.{+consonantal}*[a,ä,aa,ää]{+consonantal}*"
      allCorpusAAe = filterTableByPattern allCorpusAAePattern fd
      allCorpusAAeDisyll = filterTableByPattern allCorpusAAeDisyllPattern fd
  withFile "vowelfinals.txt" WriteMode (writeTable vfinals)
  summarySection vfinals
  vowelSummarySection "plain vowel structure" vfinals onlyVowels
  vowelSummarySection "plain harmonicity" vfinals harmonicity
  lv <- vowelSummarySection "last vowel" vfinals (flip lastVowel flags)
  allCorpusLv <- vowelSummarySection "last vowel" allCorpusAAe (flip lastVowel flags)
  allCorpusDisyllLv <- vowelSummarySection "last vowel" allCorpusAAeDisyll (flip lastVowel flags)
  withFile "wordfinal.txt" WriteMode $ \h ->
    hPutStrLn h "In all corpus:" >>
    writeTable allCorpusLv h >>
    hPutStrLn h "\nIn all corpus disyllabic:" >>
    writeTable allCorpusDisyllLv h >>
    hPutStrLn h "\nOnly neutral disyllabic:" >>
    writeTable lv h
  sltable <- vowelSummarySection "stem vowel & last vowel" vfinals (flip stemLastVowel flags)
  withFile "lexstats.txt" WriteMode (writeTable sltable)
  let patternannotated = annotateWithPatterns vfinals flags
      printPatternAfd :: AnnotatedFreqDist -> String -> Handle -> IO ()
      printPatternAfd afd annot h = do
        hPutStrLn h ""
        hPutStrLn h $ "=== PATTERN " ++ annot ++ " ==="
        let fd = dropAnnotation $ filterWithAnnotation (== T.pack annot) afd
        psltable <- vowelSummarySection ("stem vowel & last vowel with pattern " ++ annot) fd (flip stemLastVowel flags)
        writeTable psltable h
  withFile "lexstats-pattern.txt" WriteMode (\h ->
                                              mapM (\a -> printPatternAfd patternannotated a h) ["","1","2","3","4","5"])
  return ()



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


sectionHeader :: String -> IO ()
sectionHeader s = putStrLn s >> putStrLn "========"

dataPointInt :: String -> Int -> IO ()
dataPointInt label val = putStrLn $ label ++ ": " ++ show val

summarySection :: FreqDist -> IO ()
summarySection fd = sectionHeader "Summary" >>
                    dataPointInt "grand total" (sumTable fd)

summarizeAnderson :: FreqDist ->  [Flag] -> IO AnnotatedFreqDist
summarizeAnderson fd flags = do
  let l p = fromJust $ findPhoneme (theInventory flags) p
      gravesNotP = [l "k", l "g", l "kk", l "m", l "mm", l "ng", l "f", l "ff", l "v", l "vv", l "h", l "hh", l "j", l "jj"]
      acutesP = [l "p", l "pp", l "t", l "tt", l "d", l "dd", l "n", l "nn", l "s", l "ss", l "z", l "zz", l "l", l "ll", l "r", l "rr"]
      funGrave  = filterToken (theInventory flags) [DotF consonant, DotF vowel, AnyP gravesNotP, DotF $ mconcat [low,vowel]]
      funAcuteI = filterToken (theInventory flags) [DotF consonant, AnyP [l "i", l "ii", l "ei"], AnyP acutesP, DotF $ mconcat [low,vowel]]
      funAcuteE = filterToken (theInventory flags) [DotF consonant, AnyP [l "e", l "ee"], AnyP acutesP, DotF $ mconcat [low,vowel]]

      -- todo: QuestionF
      
      fdGrave = filterTable funGrave fd
      fdAcuteI = filterTable funAcuteI fd
      fdAcuteE = filterTable funAcuteE fd
  vowelSummarySection "with graves without [p] (Anderson: disharmonic)" fdGrave harmonicity
  vowelSummarySection "with acutes or [p] after [i(:),ei] (Anderson: disharmonic)" fdAcuteI harmonicity
  vowelSummarySection "with acutes or [p] after [e(:)] (Anderson: harmonic)" fdAcuteE harmonicity
  return $ annotateFD [("grave", funGrave), ("acute with i", funAcuteI), ("acute with e", funAcuteE)] fd

annotateWithPatterns :: FreqDist ->  [Flag] -> AnnotatedFreqDist
annotateWithPatterns fd flags =
  let patternAnnotMap :: [(Annotation, Token -> Bool)]
      patternAnnotMap = [
        (T.pack "1", fitsPattern (fromJust $ readPattern (theInventory flags) (T.pack "*{-consonantal}.j{-consonantal}.*")) flags),
        (T.pack "2", fitsPattern (fromJust $ readPattern (theInventory flags) (T.pack "*{-consonantal}.{-consonantal}.*")) flags),
        (T.pack "3", fitsPattern (fromJust $ readPattern (theInventory flags) (T.pack "*{-consonantal,+high}.[p,pp]{-consonantal}.*")) flags),
        (T.pack "4", fitsPattern (fromJust $ readPattern (theInventory flags) (T.pack "*{-consonantal}.[l,ll]{-consonantal}.*")) flags),
        (T.pack "5", fitsPattern (fromJust $ readPattern (theInventory flags) (T.pack "*{-consonantal}.[f,s,h,v]j{-consonantal}.*")) flags)
        ]
  in
   annotateFD patternAnnotMap fd


fitsPattern :: [Pattern] -> [Flag] -> Token ->  Bool
fitsPattern patt flags tkn =
  let
    phons = segment (theInventory flags) tkn
  in
   case phons of
     Just phs -> filterWord phs patt
     Nothing -> False

    
lastVowel :: Token ->  [Flag] -> String
lastVowel t flags = case segment (theInventory flags) t of
  Nothing -> "segment-error"
  Just x -> go x
    where
      isVowel ph = isPhoneme ph vowel
      go phs
        | phs == [] = "none"
        | isVowel (last phs) = phonemeName (last phs)
        | otherwise =  go (init phs)

stemLastVowel :: Token -> [Flag] -> [String]
stemLastVowel t flags = case segment (theInventory flags) t of
  Nothing -> []
  Just phs -> map phonemeName $ filter (flip isPhoneme vowel) phs
    
