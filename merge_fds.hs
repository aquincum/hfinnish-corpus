{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Hanalyze.FreqDist
import System.IO
import System.Environment
import Data.Monoid

-- |Runs the merging operation
runMerge :: FilePath -> -- ^The file name of the _output_
            [FilePath] -> -- ^File names of the _input_ files
            IO ()
runMerge out ins = do
  fds <- sequence $ fmap readFreqDist ins
  let fd = mconcat fds
  saveFreqDist fd out
  return ()

main :: IO ()
main = do
  fns <- getArgs
  if (length fns) < 2 then
    putStrLn "Usage:\nmerge_fds <output_file> [input files...]"
  else 
    runMerge (head fns) (tail fns)
