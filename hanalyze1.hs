module Main where

import System.IO
import System.Environment
import qualified Data.Map as Map
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Pattern
import Hanalyze.Phoneme
import Control.Monad
import Data.Monoid
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
  writeSummaryTable summedfd stdout >>
  putStrLn ""
  where
    summedfd = summarizeFD f fd



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
  let fdLab = filterFD (\tok -> filterToken finnishInventory [DotF consonant, DotF vowel, DotF labial, DotF $ mconcat [low,vowel]] tok) fd
  let fdCor = filterFD (\tok -> filterToken finnishInventory [DotF consonant, DotF vowel, DotF coronal, DotF $ mconcat [low,vowel]] tok) fd
  let fdVel = filterFD (\tok -> filterToken finnishInventory [DotF consonant, DotF vowel, DotF velar, DotF $ mconcat [low,vowel]] tok) fd      
  vowelSummarySection "with labials" fdLab harmonicity
  vowelSummarySection "with coronals" fdCor harmonicity
  vowelSummarySection "with velars" fdVel harmonicity
  --  saveFreqDist fd' a"test.out"
  return ()
