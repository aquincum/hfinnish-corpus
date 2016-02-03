{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Writer
import           Data.List (intersperse)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Random
import           Data.Random.Source.DevRandom
import qualified Data.Text as Txt
import qualified Data.Text.IO as TIO
import           Hanalyze.Chisq
import           Hanalyze.FreqDist
import           Hanalyze.Omorfi
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import           Hanalyze.ToUCLAP
import qualified Hanalyze.Token as T
import           Hanalyze.Vowels
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           Tasks.Options
import           Text.Printf



summarizeByC :: FreqDist -> IO ()
summarizeByC fd = do
  let fdLab = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF labial, DotF $ mconcat [low,vowel]]) fd
  let fdCor = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF coronal, DotF $ mconcat [low,vowel]]) fd
  let fdVel = filterTable (filterToken finnishInventory [DotF consonant, DotF vowel, DotF velar, DotF $ mconcat [low,vowel]]) fd
  vowelSummarySection "with labials" fdLab harmonicity
  vowelSummarySection "with coronals" fdCor harmonicity
  vowelSummarySection "with velars" fdVel harmonicity
  return ()



filterVowelFinals :: FreqDist -> FreqDist
filterVowelFinals = filterTable $ filterToken finnishInventory [Star, DotF vowel]


                    



main :: IO ()
main = do
  args <- getArgs
  flags <- compileOptions args

  
  when ((TaskFlag HarmSummary) `elem` flags) $ do
  when ((TaskFlag AnalyzeFile) `elem` flags) $ do
  when ((TaskFlag GenerateExamplesForGrammar) `elem` flags) $ do
    txt <- TIO.readFile $ flagGetFn flags
    let uclagr = readUCLAPLGrammar txt (flagGetFn flags)
    let patts = map generateExamples uclagr
        pattsStr = map (\p -> mconcat $ intersperse (T.pack ",") p) patts
    mapM_ (\(uc, wds) ->
            putStrLn (show uc ++ "\t" ++ (T.unpack wds))) (zip uclagr pattsStr)
        
    -- explore
    
lastVowel :: Token -> String
lastVowel t = case segment finnishInventory t of
  Nothing -> "segment-error"
  Just x -> go x
    where
      isVowel ph = isPhoneme ph vowel
      go phs
        | phs == [] = "none"
        | isVowel (last phs) = phonemeName (last phs)
        | otherwise =  go (init phs)

stemLastVowel :: Token -> [String]
stemLastVowel t = case segment finnishInventory t of
  Nothing -> []
  Just phs -> map phonemeName $ filter (flip isPhoneme vowel) phs
    
