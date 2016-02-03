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


summarizeByPattern :: FreqDist -> PhonemicInventory -> [Pattern] -> IO AnnotatedFreqDist
summarizeByPattern fd inv patt = do
  let funFits = filterToken inv patt
      funDoesntfit = not . funFits
      fdFits = filterTable funFits fd
      fdDoesntfit = filterTable funDoesntfit fd
  putStrLn $ " ========*** " ++ writePattern patt ++ " ***========"
  fitMap       <- vowelSummarySection ("fitting pattern " ++ writePattern patt) fdFits harmonicity
  doesntfitMap <- vowelSummarySection ("not fitting pattern " ++ writePattern patt) fdDoesntfit harmonicity
  let getTypeFreqs m = map (getTypeFreq . snd) (tToList m)
      table = [getTypeFreqs fitMap, getTypeFreqs doesntfitMap]
  putStrLn $ show table
  catch (do
            let xsqtest = runChiSqTest table True
            putStrLn $ "\nChi Sq = " ++ show (chisq xsqtest) ++ ", p = " ++ show (p xsqtest) ++ ", " ++ if sig xsqtest then "*SIGNIFICANT*" else "n.s."
        ) (\err -> putStrLn $"No chi square test available, " ++ (show (err::SomeException)))
  return $ annotateFD [(T.pack ("fits_"++writePattern patt), funFits),
                       (T.pack ("fitsnot_"++writePattern patt), funDoesntfit)] fd


filterVowelFinals :: FreqDist -> FreqDist
filterVowelFinals = filterTable $ filterToken finnishInventory [Star, DotF vowel]


                    



main :: IO ()
main = do
  args <- getArgs
  flags <- compileOptions args

  
  when ((TaskFlag HarmSummary) `elem` flags) $ do
    fd <- readFreqDist $ flagGetFn flags
    let doSegment wd = case segment finnishInventory wd of
          Nothing -> []
          Just s -> s
        summary = summarizeFD (abbreviateBFNs . wordHarmonies . doSegment) fd
    writeTable summary stdout
  when ((TaskFlag AnalyzeFile) `elem` flags) $ do
    fd <- readFreqDist $ flagGetFn flags
    summarySection fd
    -- get relevant bundles for vowels
    let vowels = filterInventoryByBundle finnishInventory vowel
        vowelRelevants = selectRelevantBundles vowels (flagGetMaxn flags)
        fd' = filterTable (filterToken finnishInventory [StarF consonant, DotF vowel, StarF consonant, StarF vowel]) fd
        patternGenerator fb = [StarF consonant, DotF fb, StarF consonant, StarF vowel]
        patterns = map patternGenerator vowelRelevants
    mapM_ (summarizeByPattern fd' finnishInventory) patterns
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
    
