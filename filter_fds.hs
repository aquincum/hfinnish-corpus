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
import qualified Data.Text.Lazy as T

-- |Filter a line
filterLine :: Token -> Integer -> Bool
filterLine t _ = relevantStem (segment $ T.unpack t) []

-- |Filter a FreqDist
filterFD :: FreqDist -> FreqDist
filterFD fd = FreqDist $ Map.filterWithKey (filterLine) $ getMap fd


-- |Filter a Frequency Distribution file
filterFDFile :: FilePath -> IO ()
filterFDFile fn = do
  fd <- readFreqDist fn
  saveFreqDist fd "out.txt"

main :: IO ()
main = do
  fns <- getArgs
  filterFDFile $ head fns
