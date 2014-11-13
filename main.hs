module Main where

import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import System.Environment
import Control.Applicative
import Hanalyze.FreqDist
import Hanalyze.Vowels

qsort (p:xs) = qsort [x | x<-xs, x<p] ++ [p] ++ qsort [x | x<-xs, x>=p]


  

main :: IO ()
main = do
  fns <- getArgs
  fd <- multiReadCountFreqs fns
  let filtered = FreqDist $ Map.filterWithKey (\s _ -> relevantStem (segment s) []) (getMap fd)
  saveCountFreqs filtered "out.txt"
  return ()
