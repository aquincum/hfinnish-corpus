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
  let inv = addEdgeToInventory (addFBNExclToInventory $ theInventory flags) "empty"
  convertFeaturesFile inv "sublex-features.txt"




convertCorpusFileSublexical :: PhonemicInventory -> FilePath -> FilePath -> IO ()
convertCorpusFileSublexical pi infn outfn = do
  fd <- readFreqDist infn -- this will be the frequent disyllables in 3.2
  putStrLn "Filtering out nouns"
  om <- analyseFDOmorfi fd
  let splitoff = omorfiToSFD om
      (nouns, _) = splitPOS splitoff N
      wds = map fst nouns
  putStrLn $ "Nouns filtered, n = " ++ show (length wds)
  stemsessives <- generateEssives wds
  let stems = map fst stemsessives
      essives = map snd stemsessives
  let (essivesS, problems) = runWriter $ segmentWords pi essives
  when (problems /= "") $ putStrLn ("Problems with spelling for essives:\n" ++ (Txt.unpack problems))
  let essivesOK = map (fromMaybe []) essivesS
      finalvowels = finalV essivesOK
      finalharms = map (abbreviateBFN . harmonyVP) finalvowels -- :: [Char]
      (stemsS, problems2) = runWriter $ segmentWords pi stems
      stemsTxt = map strToken stemsS
  when (problems2 /= "") $ putStrLn ("Problems with spelling for stems:\n" ++ (Txt.unpack problems2))
  let pseudosuffixeds = map (\(a,b) -> a <> (T.pack [b])) (zip stems finalharms)
      (pseudosuffixedsS, problems3) = runWriter $ segmentWords (addFBNExclToInventory pi) pseudosuffixeds
      pseudosuffixedsTxt = map strToken pseudosuffixedsS
      tooutputdoubles = filter (\(x,t) -> case Txt.uncons (Txt.reverse t) of
                                   Nothing -> False
                                   Just (ch, _) -> ch /= '!'
                               ) (zip stemsTxt pseudosuffixedsTxt)
  when (problems3 /= "") $ putStrLn ("Problems with spelling for pseudosuffixeds:\n" ++ (Txt.unpack problems3))
  let outputtxt = Txt.unlines (map (\(a,b) -> a <> "\t" <> b) tooutputdoubles)
  TxtIO.writeFile outfn outputtxt

generateEssives :: [Token] -> IO [(Token,Token)]
generateEssives tok = do
  essives <- generateForm tok "N Ess Sg"
  -- essiveswstems :: [[(Token, Token)]]
  let essiveswstems = map (\(t,s) -> map (\x -> (t,x)) s) (zip tok essives)
  return $ concat essiveswstems

finalV :: [[Phoneme]] -> [Phoneme]
finalV = map finalVIndividual
  where
    finalVIndividual ps = case ps of
      [] -> Phoneme "error" (setBundle [Feature Plus "error"])
      xs -> last xs
