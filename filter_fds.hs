module Main where

import qualified Data.Map as Map
import System.IO
import System.Environment
import Control.Monad
import Control.Exception
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Process
import Hanalyze.Progress
import Hanalyze.Phoneme
import Hanalyze.Pattern
import Hanalyze.Omorfi
import qualified Hanalyze.Token as T
import Data.Char
import Data.Maybe (isNothing, isJust, fromJust)
import System.Console.GetOpt
import System.FilePath.Posix
import Control.Concurrent

-- Option parsing:

data Flag = Stem | Omorfi deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['s'] ["stem"] (NoArg Stem) "stem and filter corpus",
  Option ['o'] ["omorfi"] (NoArg Omorfi) "only stem the corpus with Omorfi"
  ]

compileOptions :: [String] -> IO ([Flag], [String])
compileOptions args = case getOpt Permute options args of
  (o, n, []) -> return (o, n)
  (_, _, errs) -> error $ "Option parsing error: " ++ concat errs ++
                  "\n" ++ usageInfo "Usage: filter_fds [OPTION...] files" options




-- |In my dissertation, I'll be looking at C[i,e,ie]C[a,ä] forms and more generally C[i,e,ie]CV forms.
-- First try: C[i,e,ie,ei]C[a,ä] stems are relevant
--
-- Example:
--
-- >>> relevantStem (segment "aliaala") []
-- False
relevantStem :: [Phoneme] -- ^token recursively folded left-to-right
                -> [Phoneme] -- ^saved list of vowels so far
                -> Bool  -- ^the return value
relevantStem [] [v1,v2] = True
relevantStem [] _ = False
relevantStem (h:t) l
  | isNothing $ harmonyV $ head (phonemeName h) = relevantStem t l
relevantStem (h:t) [v1,v2] = (isNothing . harmonyV $ head (phonemeName h)) && relevantStem t [v1,v2]
relevantStem (h:t) [v1] = (phonemeName h `elem` ["a","aa","ä","ää"]) && relevantStem t [v1,h] 
relevantStem (h:t) [] = (phonemeName h `elem` ["e","i","ee","ii","ei","ie"]) && relevantStem t [h]

-- |Filter a token based on relevance 
filterTokenRelevant :: Token ->  Bool
filterTokenRelevant t = case segment finnishInventory t of
  Nothing -> False
  Just x -> relevantStem x []

-- |Filter a token based on relevance -- advanced, stemmed version
stemFilterTokenRelevant :: Token ->  Bool
stemFilterTokenRelevant t = case segment finnishInventory t of
  Nothing -> False
  Just x -> filterWord x pattern
 where
   pattern = [StarF $ setBundle [fCons],
              DotF $ setBundle [fFront, minus fLow, minus fRounded],
              Star
              ]
y   l p = fromJust $ findPhoneme finnishInventory p

-- |Cleaning up non-alphanumeric symbols. Could get more complicated
cleanupWord :: Token -> Token
cleanupWord = T.filter (`elem` "abcdefghijklmnopqrstuvwxyzäö")

stemFDFile :: Flag -- ^Input flag -- Omorfi or Stem
              -> FilePath -- ^Input file
              -> IO ()
stemFDFile fl fn = do
  let saveprefix = "filtered2_"
      (dirname,fname) = splitFileName fn
      savefn = dirname </> (saveprefix ++ fname)
  fd <- readFreqDist fn
  let cleaned = filterTable stemFilterTokenRelevant . cleanupTable cleanupWord $ fd
  om <- analyseFDOmorfi cleaned
  let om' = filterByValTable (any getKnown) om
      stemmed = getStems om'
      filteredStemmed | fl == Stem = filterTable filterTokenRelevant stemmed
                      | otherwise = stemmed
  saveTable filteredStemmed savefn


omorfFDFile :: FilePath -> IO ()
omorfFDFile fn = do
  let saveprefix = "filtered3_"
      (dirname,fname) = splitFileName fn
      savefn = dirname </> (saveprefix ++ fname)
  fd <- readFreqDist fn
  let cleaned = filterTable stemFilterTokenRelevant . cleanupTable cleanupWord $ fd
  om <- analyseFDOmorfi cleaned
  let om' = filterByValTable (any getKnown) om
      stemmed = getStems om'
      filteredStemmed = filterTable filterTokenRelevant stemmed
  saveTable filteredStemmed savefn


main :: IO ()
main = do
  args <- getArgs
  (flags, fns) <- compileOptions args
  when (length fns < 1) (error "No files specified")
  when (Stem `elem` flags && Omorfi `elem` flags) (error "Choose either -s or -o")
  progVar <- initializeProgVar fns
  let filterAction | Stem `elem` flags = stemFDFile Stem
                   | Omorfi `elem` flags = stemFDFile Stem
                   | otherwise = filterFDFile filterTokenRelevant cleanupWord
      runOneFile fn = do
        filterAction fn
        incrementProgVar progVar
        printWithProgVal printProgress progVar >>= putStrLn
  mapM_ filterAction fns
  return ()
