{-# LANGUAGE OverloadedStrings #-}
module Tasks.Sublexical (task) where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Maybe
import qualified Data.Text as Txt
import qualified Data.Text.IO as TxtIO
import           Hanalyze.FreqDist
import           Hanalyze.Omorfi
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import           Hanalyze.ToUCLAP
import qualified Hanalyze.Token as T
import           Hanalyze.Vowels
import           System.Directory
import           System.IO
import           Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "sublexical"
         "Create a training file for Michael Becker's sublexical learner."

doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  convertCorpusFileSublexical (theInventory flags) infn "sublex-training.txt"

convertCorpusFileSublexical :: PhonemicInventory -> FilePath -> FilePath -> IO ()
convertCorpusFileSublexical pi infn outfn = do
  fd <- readFreqDist infn -- this will be the frequent disyllables in 3.2
  let wds = fdKeys fd
  essives <- generateEssives wds
  let (essivesS, problems) = runWriter $ segmentWords pi essives
  when (problems /= "") $ putStrLn "Problems with essives:\n" >> putStrLn (Txt.unpack problems)
  let essivesOK = catMaybes essivesS
      finalvowels = finalV essivesOK
      finalharms = map (abbreviateBFN . harmonyVP) finalvowels -- :: [Char]
      pseudosuffixeds = map (\(a,b) -> a <> (T.pack [b])) (zip wds finalharms)
      outputtkn = T.unlines (map (\(a,b) -> a <> "\t" <> b) (zip wds pseudosuffixeds))
      outputtxt = T.getText outputtkn
  TxtIO.writeFile outfn outputtxt

generateEssives :: [Token] -> IO [Token]
generateEssives tok = do
  stems <- generateForm tok "N Ess Sg"
  stems' <- filterM (\(t,s) ->
                if length s > 1 then do
                    putStrLn $ "Multiple analyses for " ++ (T.unpack t)
                    return False
                else (return True)
                ) (zip tok stems)
  return $ concat (map snd stems')

finalV :: [[Phoneme]] -> [Phoneme]
finalV = map finalVIndividual
  where
    finalVIndividual ps = case ps of
      [] -> Phoneme "error" (setBundle [Feature Plus "error"])
      xs -> last xs
