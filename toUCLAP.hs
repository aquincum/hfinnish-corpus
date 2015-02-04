-- |Convert a Hanalyze style corpus with feature information to a format
-- usable by the Hayes and Wilson's UCLA Phonotactic Learner

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hanalyze.Phoneme
import Hanalyze.FreqDist
import Control.Monad (when)
import System.Environment

-- need a features file and a training file



convertFeatures :: PhonemicInventory -> T.Text
convertFeatures pi =
  let
    allFeatureNames = listFeatures pi
    strPhoneme :: Phoneme -> T.Text
    strPhoneme p =
      let fb = featureBundle p
          strFValue :: String -> T.Text
          strFValue sfname =
            let mfeat = findInBundle sfname fb
            in
             case mfeat of
               Just f -> T.pack $ show $ plusMinus f
               Nothing -> "0"
          featureStrs = map strFValue allFeatureNames
      in
       T.intercalate "\t" [T.pack $ phonemeName p, T.intercalate "\t" featureStrs]
    allPhonemes = T.intercalate "\n" (map strPhoneme pi)
    titleLine = T.append "\t" $ T.intercalate "\t" $ map T.pack allFeatureNames
  in
    T.intercalate "\n" [titleLine, allPhonemes]

convertFeaturesFile :: PhonemicInventory -> FilePath -> IO ()
convertFeaturesFile pi fn = (TIO.writeFile fn $ convertFeatures pi) >>
                            putStrLn ("Features file " ++ fn ++ " saved.")


convertCorpus :: PhonemicInventory -> FreqDist -> T.Text
convertCorpus pi fd =
  let
    tokens = fdKeys fd
    msegmenteds = map (segment pi) tokens
    strToken :: Maybe [Phoneme] -> T.Text
    strToken mp = case mp of
      Just phs -> T.intercalate " " (map (T.pack . phonemeName) phs)
      Nothing -> ""
    lines = filter (not . T.null) (map strToken msegmenteds)
  in
   T.intercalate "\n" lines

convertCorpusFile :: PhonemicInventory -> FilePath -> FilePath -> IO ()
convertCorpusFile pi infn outfn = do
  fd <- readFreqDist infn
  TIO.writeFile outfn (convertCorpus pi fd)
  putStrLn $ "Corpus (training) file " ++ outfn ++ " saved."


usage = "USAGE: toUCLAP <corpus file>\n" ++
        "outputs a UCLAPL-compatible Features.txt and Training.txt based on a" ++
        "frequency distribution file (and the Finnish inventory)\n" ++
        "corpus file = a frequency distribution file\n"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error usage)
  let infn = head args
  convertFeaturesFile finnishInventory "Features.txt"
  convertCorpusFile finnishInventory infn "Training.txt"
