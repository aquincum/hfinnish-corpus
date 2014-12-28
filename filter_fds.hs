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

data Flag = Stem deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['s'] ["stem"] (NoArg Stem) "filter on stemmed corpus"
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
relevantStem (h:t) [v1] = ((phonemeName h) `elem` ["a","aa","ä","ää"]) && relevantStem t [v1,h] 
relevantStem (h:t) [] = ((phonemeName h) `elem` ["e","i","ee","ii","ei","ie"]) && relevantStem t [h]

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
   l p = fromJust $ findPhoneme finnishInventory p

-- |Cleaning up non-alphanumeric symbols. Could get more complicated
cleanupWord :: Token -> Token
cleanupWord = T.filter (`elem` "abcdefghijklmnopqrstuvwxyzäö")

stemFDFile :: FilePath -> IO ()
stemFDFile fn = do
  let saveprefix = "filtered2_"
      (dirname,fname) = splitFileName fn
      savefn = dirname </> (saveprefix ++ fname)
  fd <- readFreqDist fn
  let cleaned = filterTable stemFilterTokenRelevant . cleanupFD cleanupWord $ fd
  om <- analyseFDOmorfi cleaned
  let om' = filterByValTable (\omi -> any getKnown omi) om
      stemmed = getStems om'
      filteredStemmed = filterTable filterTokenRelevant stemmed
  saveTable filteredStemmed savefn

main :: IO ()
main = do
  args <- getArgs
  (flags, fns) <- compileOptions args
  when (length fns < 1) (error "No files specified")
  progVar <- initializeProgress fns >>= newMVar
  let filterAction = if Stem `elem` flags
                     then stemFDFile
                     else (filterFDFile filterTokenRelevant cleanupWord)
      runOneFile fn = do
        filterAction fn
        progval <- takeMVar progVar
        let progval' = incrementProgress progval
        printProgress progval'
        putMVar progVar progval'
  mapM_ filterAction fns
  return ()
