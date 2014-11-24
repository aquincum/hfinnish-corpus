{-# LANGUAGE DoAndIfThenElse,BangPatterns #-}
module Main where

import Hanalyze.FreqDist
import System.IO
import System.Environment
import Data.Monoid
import Data.List
import Control.Applicative
import Control.DeepSeq

-- |Runs the merging operation
runMerge :: FilePath -> -- ^The file name of the _output_
            [FilePath] -> -- ^File names of the _input_ files
            IO ()
runMerge out ins = do
  --fds <- sequence $ fmap readFreqDist ins
  --let fd = fds `seq` mconcat fds
--  fd <- sequence (fmap readFreqDist ins) >>= foldl' (\a b -> (liftA2 mappend) a (return b)) (return fdEmpty)
  fd <- foldl (\a b -> (liftA2 mappend) a (readFreqDist b)) (return fdEmpty) ins 
  saveFreqDist fd out
  return ()

main :: IO ()
main = do
  fns <- getArgs
  if (length fns) < 2 then
    putStrLn "Usage:\nmerge_fds <output_file> [input files...]"
  else 
    runMerge (head fns) (tail fns)
