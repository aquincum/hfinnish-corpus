module Main where

import qualified Data.Map as Map
import System.IO
import System.Environment
import qualified Control.Monad as M
import Control.Exception
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Process
import Hanalyze.Progress
import qualified Data.Text as T
import Data.Char


-- |Cleaning up non-alphanumeric symbols. Could get more complicated
cleanupWord :: T.Text -> T.Text
cleanupWord = T.filter isAlphaNum

-- |Filter a line
filterLine :: Token -> Int -> Bool
filterLine t _ = (relevantStem . segment) (T.unpack t) []

  --(relevantStem  . cleanupWord)  (segment $ T.unpack t) []

-- |Filter a FreqDist
filterFD :: FreqDist -> FreqDist
filterFD fd = FreqDist $ Map.filterWithKey (filterLine) $ getMap fd


-- |Filter a Frequency Distribution file
filterFDFile :: FilePath -> IO ()
filterFDFile fn = do
  fd <- readFreqDist fn
  saveFreqDist (filterFD . cleanupFD cleanupWord $ fd) ("filtered_"++fn)



main :: IO ()
main = do
  fns <- getArgs
  sequence $ map filterFDFile fns
  return ()
