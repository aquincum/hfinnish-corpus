-- |Convert a Hanalyze style corpus with feature information to a format
-- usable by the Hayes and Wilson's UCLA Phonotactic Learner

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Hanalyze.ToUCLAP (
  convertFeatures, convertFeaturesFile,
  convertCorpus, convertCorpusFile,
  convertCorpusFileSublexical,
  createNatClassFile,
  generateCICAWugs1,
  generateCICAWugsCluster,
  generateCICAWugsHiatus
  )
       where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Hanalyze.Phoneme
import Hanalyze.FreqDist
import Hanalyze.Pattern
import Text.Parsec
import qualified Data.Map.Strict as Map
import qualified Hanalyze.Token as Tkn
import Control.Monad (when)
import Control.Monad.Writer
import Data.Maybe (isNothing, mapMaybe, fromJust)
import System.Environment
import System.Console.GetOpt
import Data.Monoid

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


segmentWords :: PhonemicInventory -> [Token] -> Writer T.Text ([Maybe [Phoneme]])
segmentWords pi tokens = mapM (\tok -> do
                                  let seg = segment pi tok
                                  when (isNothing seg) $ tell (Tkn.getText tok <> "\n")
                                  return seg
                              ) tokens

strToken :: Maybe [Phoneme] -> T.Text
strToken mp = case mp of
  Just phs -> T.intercalate " " (map (T.pack . phonemeName) phs)
  Nothing ->  ""


convertCorpus :: PhonemicInventory -> FreqDist -> Writer T.Text T.Text
convertCorpus pi fd =
  let
    tokens = fdKeys fd
    lines = do
      segmenteds <- segmentWords pi tokens
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

convertCorpusFileSublexical :: PhonemicInventory -> FilePath -> FilePath -> IO ()
convertCorpusFileSublexical pi infn outfn = do
  fd <- readFreqDist infn
{-  let finaltable = annotateFD [(Tkn.pack "a", \t -> T.last (Tkn.getText t) == 'a'),
                               (Tkn.pack "ae", \t -> T.last (Tkn.getText t) == 'ä')]
                               fd
      withnofinaltable = AnnotatedFreqDist $ Map.mapKeys (Tkn.Tok . T.init . Tkn.getText) (tGetMap finaltable)
      allnonfinal = filterWithAnnotation (\a -> Tkn.length a > 0) withnofinaltable
      (txtnofinal, problems) = runWriter $ convertCorpus pi (dropAnnotation allnonfinal)-}
  let wds = fdKeys fd
      (segmap, problems) = runWriter $ segmentWords pi wds
  when (problems /= "") $ putStrLn "Problems:\n" >> putStrLn (T.unpack problems)
  -- Below: only final vowels.
  let monlyvfinalmap = filter (\mphons -> case mphons of
                                 Just phons -> phonemeName (last phons) `elem` ["a", "aa", "ä", "ää"]
                                 _ -> False) segmap
      onlyvfinalmap = map fromJust monlyvfinalmap
      nofinalvmap = map init onlyvfinalmap
      zipped = zip nofinalvmap onlyvfinalmap
      txt = T.unlines $ map (\(a,b) -> T.concat [strToken (Just a), T.pack "\t",strToken (Just b)]) zipped
  putStrLn $ "Corpus (training) file " ++ outfn ++ " saved."
  TIO.writeFile outfn txt

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


-- |Too much, not using this for now
generateUCLAWugs :: [Pattern] -> [[Phoneme]]
generateUCLAWugs up =
  let 
    patt1 = [DotF vowel, DotF consonant, DotF vowel]
    patt2 = [DotF vowel, DotF consonant, DotF consonant, DotF vowel]
    patt3 = [DotF vowel, DotF consonant, DotF vowel, DotF consonant]
    patt4 = [DotF vowel, DotF consonant, DotF consonant, DotF vowel, DotF consonant]
    vPatts = [patt1, patt2, patt3, patt4]
    patts = vPatts ++ map (DotF consonant:) vPatts
    wds = concatMap (generateOverlappedPatterns finnishInventory up) patts
  in
   wds

-- |Internal, for debugging. Number of segments or combinations,
-- based on pattern read from string
nSegments1 :: String -> Int
nSegments1 patt = length $ fromJust $ generatePattern finnishInventory (fromJust $readPattern finnishInventory  (Tkn.pack patt))
-- |Internal, for debugging. Number of segments,
-- based on 1 featurebundle
nSegments2 :: FeatureBundle -> Int
nSegments2 fb = length $ fromJust $ generatePattern finnishInventory [DotF fb]



vI = DotF (vowel <> high <> mid <> unrounded) -- 6
vA = DotF (vowel <> short <> low) -- 2
singletonCons = DotF (consonant <> singleton) -- 18
anyCons = DotF consonant -- 37
createPatt patt = case generatePattern finnishInventory patt of
  Just xs -> xs
  Nothing -> []

generateCICAWugs1 :: [[Phoneme]]
generateCICAWugs1 =
  let
    patt1 = [singletonCons, vI, anyCons, vA] -- 7992
    patt2 = [vI, anyCons, vA] -- 1332
    patts = [patt1, patt2]
  in
   concatMap createPatt patts

generateCICAWugsCluster :: [[Phoneme]]
generateCICAWugsCluster =
  let
    patt1 = [singletonCons, vI, singletonCons, singletonCons, vA] -- 69984
    patt2 = [vI, singletonCons, singletonCons, vA] -- 23328
    patts = [patt1, patt2]
  in
   concatMap createPatt patts

generateCICAWugsHiatus :: [[Phoneme]]
generateCICAWugsHiatus =
  let
    patt1 = [singletonCons, vI, vA] -- 
    patt2 = [vI, vA] -- 
    patts = [patt1, patt2]
  in
   concatMap createPatt patts



createNatClassFile :: PhonemicInventory -> FilePath -> IO ()
createNatClassFile pi fp = 
  let natclasses = selectRelevantBundles pi 4
      str = T.concat $ map (\x -> "Novel of size " `T.append` T.pack (show (length x)) `T.append` ": " `T.append` T.intercalate (T.pack ",") (map (T.pack . show) x)) (map getBundle natclasses)
  in
   TIO.writeFile fp str

{-
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) (error usage)
  let infn = head args
  convertFeaturesFile finnishInventoryWithEdges "Features.txt"
  convertCorpusFile finnishInventory infn "Training.txt"
  createNatClassFile finnishInventoryWithEdges "NatClassesFile.txt"
-}
{-
Plan to automatize:

- do features/training
+ ä > ae
+ create temp files
  + tempFeatureList = simple list of features
  + tempFeatureChart = simple description of inv
  + NatClassesFile.txt = HARDER, need natclasses
  + tempLearningFile = simple freqdist w one
  + tempTestingFile.tmp = simple list
  + tempTierGramSizeFile.tmp = "3" mondjuk
+ run baby 10 times
+ merge
-}
