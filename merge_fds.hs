{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Hanalyze.FreqDist
import System.IO
import System.Environment
import Data.Monoid
import Data.List
import Control.Applicative

-- |Runs the merging operation
runMerge :: FilePath -> -- ^The file name of the _output_
            [FilePath] -> -- ^File names of the _input_ files
            IO ()
runMerge out ins = do
  fd <- foldl (\a b -> liftA2 mappend a (readFreqDist b)) (return fdEmpty) ins 

  -- placeholder for parallel
  --fds <- sequence $ runEval $ rpar $ fmap readFreqDist ins
  --let fd = runEval $ rpar $ mconcat fds
  --below is cooler, haskellier and much less clear!
  --unfortunately parallelism doesn't help. It does hurt,
  --though, recommend running with -N2
  --fd <- runEval $ (sequence $ fmap (rpar . readFreqDist) ins) >>=
  --                (\fds -> rpar $ liftM mconcat $ sequence $ fds)

  saveFreqDist fd out
  return ()

main :: IO ()
main = do
  fns <- getArgs
  if length fns < 2 then
    putStrLn "Usage:\nmerge_fds <output_file> [input files...]"
  else 
    runMerge (head fns) (tail fns)
