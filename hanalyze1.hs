{-# LANGUAGE OverloadedStrings #-}

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
import qualified Hanalyze.Token as T

sectionHeader :: String -> IO ()
sectionHeader s = putStrLn s >> putStrLn "========"

dataPointInt :: String -> Int -> IO ()
dataPointInt label val = putStrLn $ label ++ ": " ++ show val

summarySection :: FreqDist -> IO ()
summarySection fd = sectionHeader "Summary" >>
                    dataPointInt "grand total" (sumFD fd)


vowelSummarySection :: (Show x, Eq x) => String -> FreqDist -> (Token -> x) -> IO ()
vowelSummarySection str fd f =
  sectionHeader ("Vowel structure summary -- " ++ str)  >>
  writeTable summedfd stdout >>
  putStrLn ""
  where
    summedfd = summarizeFD f fd

summarizeByC :: FreqDist -> IO ()
summarizeByC fd = do
  let fdLab = filterTable (\tok -> filterToken finnishInventory [DotF consonant, DotF vowel, DotF labial, DotF $ mconcat [low,vowel]] tok) fd
  let fdCor = filterTable (\tok -> filterToken finnishInventory [DotF consonant, DotF vowel, DotF coronal, DotF $ mconcat [low,vowel]] tok) fd
  let fdVel = filterTable (\tok -> filterToken finnishInventory [DotF consonant, DotF vowel, DotF velar, DotF $ mconcat [low,vowel]] tok) fd      
  vowelSummarySection "with labials" fdLab harmonicity
  vowelSummarySection "with coronals" fdCor harmonicity
  vowelSummarySection "with velars" fdVel harmonicity

summarizeAnderson :: FreqDist -> IO AnnotatedFreqDist
summarizeAnderson fd = do
  let l p = fromJust $ findPhoneme finnishInventory p
      gravesNotP = [l "k", l "g", l "kk", l "m", l "mm", l "ng", l "f", l "ff", l "v", l "vv", l "h", l "hh", l "j", l "jj"]
      acutesP = [l "p", l "pp", l "t", l "tt", l "d", l "dd", l "n", l "nn", l "s", l "ss", l "z", l "zz", l "š", l "šš", l "ž", l "žž", l "l", l "ll", l "r", l "rr"]
      funGrave tok = filterToken finnishInventory [DotF consonant, DotF vowel, AnyP gravesNotP, DotF $ mconcat [low,vowel]] tok
      funAcuteI tok = filterToken finnishInventory [DotF consonant, AnyP [l "i", l "ii", l "ei"], AnyP acutesP, DotF $ mconcat [low,vowel]] tok
      funAcuteE tok = filterToken finnishInventory [DotF consonant, AnyP [l "e", l "ee"], AnyP acutesP, DotF $ mconcat [low,vowel]] tok
      fdGrave = filterTable funGrave fd
      fdAcuteI = filterTable funAcuteI fd
      fdAcuteE = filterTable funAcuteE fd
  vowelSummarySection "with graves without [p] (Anderson: disharmonic)" fdGrave harmonicity
  vowelSummarySection "with acutes or [p] after [i(:),ei] (Anderson: disharmonic)" fdAcuteI harmonicity
  vowelSummarySection "with acutes or [p] after [e(:)] (Anderson: harmonic)" fdAcuteE harmonicity
  return $ annotateFD [("grave", funGrave), ("acute with i", funAcuteI), ("acute with e", funAcuteE)] fd



main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    progn <- getProgName
    error $ "Usage: " ++ progn ++ " freqdist_file"
  fd <- readFreqDist $ head args
  {-putStrLn "Omorfi analysis"
  om <- analyseFDOmorfi fd
  let om' = filterByValTable (\omi -> any getKnown omi) om
  saveTable om' "omorfied.out"
  let stemmed = getStems om'
  saveTable stemmed "stemmed.out" -- lol -}
  summarySection fd
  vowelSummarySection "plain vowel structure" fd onlyVowels
  vowelSummarySection "plain harmonicity" fd harmonicity
  putStrLn "# MAIN FD"
  withFile "summary_annot_fd.txt" WriteMode (\h -> do
                                              annotfd <- summarizeAnderson fd
                                              writeTable annotfd h
                                              )
  putStrLn "# ONLY >10 FREQ FD"      
  summarizeAnderson $ filterByFreqFD (> 10) fd
  putStrLn "# ONLY >50 FREQ FD"      
  summarizeAnderson $ filterByFreqFD (> 50) fd
  putStrLn "# ONLY >100 FREQ FD"      
  summarizeAnderson $ filterByFreqFD (> 100) fd
  --  saveTable fd' a"test.out"
  return ()
