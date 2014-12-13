module Main where

import System.IO
import System.Environment
import qualified Data.Map as Map
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Pattern
import Hanalyze.Phoneme
import Control.Monad
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
  writeCountFreqs summedfd stdout >>
  putStrLn ""
  where
    summedfd = splitByFD f fd



main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    progn <- getProgName
    error $ "Usage: " ++ progn ++ " freqdist_file"
  fd <- readFreqDist $ head args
  summarySection fd
  vowelSummarySection "plain vowel structure" fd onlyVowels
  vowelSummarySection "plain harmonicity" fd harmonicity
  let fd' = filterFD (\tok -> case segment finnishInventory tok of
                         Nothing -> False
                         Just seg -> filterWord seg [Star, F vowel, F labial, Star]
                     ) fd
  vowelSummarySection "with labials" fd' harmonicity
  return ()
