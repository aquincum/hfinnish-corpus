{-# LANGUAGE DoAndIfThenElse #-}
module Tasks.MergeFDs (task) where

import Hanalyze.FreqDist
import Data.Monoid
import Control.Applicative
import Tasks.Task

task :: Task
task = Task
  doTask
  (Just FileName)
  "mergefds"
  "Merges freqdists into a summary one. The first file name given will be the output freqdist, while all the rest files will be merged into this one."


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

  saveTable fd out
  return ()

doTask :: [Flag] -> IO ()
doTask flags = do
  let fns = map (\(FileName f) -> f) $ getAllFlags flags FileName
  if length fns < 2 then
    putStrLn "Usage:\nhanalyze mergefds <output_file> [input files...]"
  else 
    runMerge (head fns) (tail fns)
