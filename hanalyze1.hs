module Main where

import System.IO
import System.Environment
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

sectionHeader :: String -> IO ()
sectionHeader s = putStrLn s >> putStrLn "========"

dataPointInt :: String -> Int -> IO ()
dataPointInt label val = putStrLn $ label ++ ": " ++ show val

summarySection :: FreqDist -> IO ()
summarySection fd = sectionHeader "Summary" >>
                    dataPointInt "grand total" (sumFD fd)


vowelSummarySection :: FreqDist -> IO ()
vowelSummarySection fd = sectionHeader "Vowel structure summary" >>
                         writeCountFreqs summedfd stdout >>
                         saveFreqDist summedfd "test.out"
                         where
                           summingf = onlyVowels . T.unpack
                           summedfd = splitByFD summingf fd
--                           classes = fdKeys summedfd
--                           printClass key = dataPointInt (T.unpack key)



main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    progn <- getProgName
    error $ "Usage: " ++ progn ++ " freqdist_file"
  fd <- readFreqDist $ head args
  summarySection fd
  vowelSummarySection fd
  return ()
