module Main where

import System.IO
import System.Environment
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Control.Monad
import qualified Hanalyze.Token as T

sectionHeader :: String -> IO ()
sectionHeader s = putStrLn s >> putStrLn "========"

dataPointInt :: String -> Int -> IO ()
dataPointInt label val = putStrLn $ label ++ ": " ++ show val

summarySection :: FreqDist -> IO ()
summarySection fd = sectionHeader "Summary" >>
                    dataPointInt "grand total" (sumFD fd)


vowelSummarySection :: (Show x, Eq x) => FreqDist -> (String -> x) -> IO ()
vowelSummarySection fd f =
  sectionHeader "Vowel structure summary" >>
  writeCountFreqs summedfd stdout
  where
    summingf = f . T.unpack
    summedfd = splitByFD summingf fd



main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    progn <- getProgName
    error $ "Usage: " ++ progn ++ " freqdist_file"
  fd <- readFreqDist $ head args
  summarySection fd
  vowelSummarySection fd onlyVowels
  vowelSummarySection fd (segment . onlyVowels)
  vowelSummarySection fd harmonicity
  return ()
