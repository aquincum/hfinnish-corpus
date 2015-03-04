-- |Convert a Hanalyze style corpus with feature information to a format
-- usable by the Hayes and Wilson's UCLA Phonotactic Learner

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hanalyze.Phoneme
import Hanalyze.FreqDist
import Hanalyze.Pattern
import Text.Parsec
import qualified Hanalyze.Token as Tkn
import Control.Monad (when)
import Control.Monad.Writer
import Data.Maybe (isNothing, mapMaybe)
import System.Environment

-- need a features file and a training file

type UCLAParser st x = Parsec T.Text st x


#ifndef CABAL_INSTALL
-- Need this for GHCi
instance (Monad m) => Stream T.Text m Char where
    uncons = return . T.uncons
#endif

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


convertCorpus :: PhonemicInventory -> FreqDist -> Writer T.Text T.Text
convertCorpus pi fd =
  let
    tokens = fdKeys fd
    msegmenteds :: Writer T.Text ([Maybe [Phoneme]])
    msegmenteds = mapM (\tok -> do
                           let seg = segment pi tok
                           when (isNothing seg) $ tell (Tkn.getText tok)
                           return seg
                      ) tokens
    strToken :: Maybe [Phoneme] -> T.Text
    strToken mp = case mp of
      Just phs -> T.intercalate " " (map (T.pack . phonemeName) phs)
      Nothing ->  ""
    lines = do
      segmenteds <- msegmenteds
      return $ filter (not . T.null) (map strToken segmenteds)
  in
   lines >>= return . (T.intercalate "\n")
--   return $ T.intercalate "\n" lines

convertCorpusFile :: PhonemicInventory -> FilePath -> FilePath -> IO ()
convertCorpusFile pi infn outfn = do
  fd <- readFreqDist infn
  let (fi, problems) = runWriter $ convertCorpus pi fd
  TIO.writeFile outfn fi
  putStrLn $ "Corpus (training) file " ++ outfn ++ " saved.\n\nProblems:\n"
  putStrLn (T.unpack problems)


usage = "USAGE: toUCLAP <corpus file>\n" ++
        "outputs a UCLAPL-compatible Features.txt and Training.txt based on a" ++
        "frequency distribution file (and the Finnish inventory)\n" ++
        "corpus file = a frequency distribution file\n"


parseUCLAPConstraint :: T.Text -> [Pattern]
parseUCLAPConstraint txt = case parse doParseUCLAPConstraint "UCLA constraint reading" txt of
  Left e -> []
  Right x -> x

doParseUCLAPConstraint :: UCLAParser st [Pattern]
doParseUCLAPConstraint = do
  char '*'
  many1 doParseOneConstraint

doParseOneConstraint :: UCLAParser st Pattern
doParseOneConstraint = do
  char '['
  feats <- sepBy1 doParseOneFeature (char ',')
  char ']'
  return $ DotF $ setBundle feats
  

doParseOneFeature :: UCLAParser st Feature
doParseOneFeature = do
  pm <- char '+' <|> char '-'
  fn <- many1 $ noneOf "[,]"
  return $ Feature (if pm == '+' then Plus else Minus) fn


-- |Too much
generateUCLAWugs :: [Pattern] -> [[Phoneme]]
generateUCLAWugs up =
  let 
    patt1 = [DotF vowel, DotF consonant, DotF vowel]
    patt2 = [DotF vowel, DotF consonant, DotF consonant, DotF vowel]
    patt3 = [DotF vowel, DotF consonant, DotF vowel, DotF consonant]
    patt4 = [DotF vowel, DotF consonant, DotF consonant, DotF vowel, DotF consonant]
    vPatts = [patt1, patt2, patt3, patt4]
    patts = vPatts ++ map (DotF consonant:) vPatts
    wds = concatMap (generateOverlappedPatterns finnishInventory up) vPatts
  in
   wds


main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error usage)
  let infn = head args
  convertFeaturesFile finnishInventory "Features.txt"
  convertCorpusFile finnishInventory infn "Training.txt"



  
