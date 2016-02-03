{-# LANGUAGE DoAndIfThenElse #-}
module Tasks.MergeFDs (task) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Hanalyze.FreqDist
import Tasks.Task

task :: Task
task = Task
  doTask
  (Just FileName)
  "mergefds"
  "Merges freqdists into a summary one. The first file name given will be the output freqdist, while all the rest files will be merged into this one. An optional argument is --filetype/-t, with which you can specify what kind of freqdist you are dealing with (summarytable, freqdist)."


-- |Runs the merging operation
runMerge :: (Table t a, Monoid t) =>
            FilePath -> -- ^The file name of the _output_
            [FilePath] -> -- ^File names of the _input_ files
            (FilePath -> IO t) -> -- ^The 'Table'-specific reader function
            IO ()
runMerge out ins readfunc = do
  fd <- foldl (\a b -> liftA2 mappend a (readfunc b)) (return tEmpty) ins 

  -- placeholder for parallel
  --fds <- sequence $ runEval $ rpar $ fmap readFreqDist ins
  --let fd = runEval $ rpar $ mconcat fds
  --below is cooler, haskellier and much less clear!
  --unfortunately parallelism doesn't help. It does hurt,
  --though, recommend running with -N2
  --fd <- runEval $ (sequence $ fmap (rpar . readFreqDist) ins) >>=
  --                (\fds -> rpar $ liftM mconcat $ sequence $ fds)

  saveTable fd out
  return ()

doTask :: [Flag] -> IO ()
doTask flags = do
  when (length fns < 2) $ error "Usage:\nhanalyze mergefds <output_file> [input files...]"
  runner
  where
    fns = map (\(FileName f) -> f) $ getAllFlags flags FileName
    filetype = case getFlag flags FileType of
        Nothing -> FFreqDist
        Just (FileType f) -> f
    runner = case filetype of
        FFreqDist -> runMerge (head fns) (tail fns) readFreqDist
        FSummaryTable -> runMerge (head fns) (tail fns) readSummaryTable

