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
import Data.Monoid
import System.Console.GetOpt
import System.FilePath.Posix
import Control.Concurrent

-- Option parsing:

data Flag = Stem | Omorfi | Clean deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
  Option ['s'] ["stem"] (NoArg Stem) "stem and filter corpus",
  Option ['o'] ["omorfi"] (NoArg Omorfi) "only stem the corpus with Omorfi",
  Option ['c'] ["clean"] (NoArg Clean) "only clean the corpus (don't stem)"
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
  {-
stemFilterTokenRelevant :: Token ->  Bool
stemFilterTokenRelevant t = filterTableByPattern  [StarF $ setBundle [fCons],
              DotF $ setBundle [fFront, minus fLow, minus fRounded],
              Star
              ] t-}

-- |Cleaning up non-alphanumeric symbols. Could get more complicated
cleanupWord :: Token -> Token
cleanupWord = T.filter (`elem` "abcdefghijklmnopqrstuvwxyzäö")

stemFDFile :: Flag -- ^Input flag -- Omorfi or Stem or Clean
              -> FilePath -- ^Input file
              -> IO ()
stemFDFile fl fn = do
  let saveprefix | fl == Stem = "filtered2_"
                 | fl == Omorfi = "filtered3_"
                 | fl == Clean = "cleaned_"
      (dirname,fname) = splitFileName fn
      savefn = dirname </> (saveprefix ++ fname)
  fd <- readFreqDist fn
  putStrLn $ "FreqDist read, " ++ (show $ tSize fd) ++ " tokens."
  -- putStrLn $ show $ tToList fd
  let cleaned = cleanupTable cleanupWord $ fd
  putStrLn $ "FreqDist cleaned, " ++ (show $ tSize cleaned) ++ " tokens."
  -- putStrLn $ show $ tToList cleaned
  let relPattern = [StarF $ setBundle [fCons],
                    DotF $ setBundle [fFront, minus fLow, minus fRounded],
                    Star
                   ] 
  let cleaned' = if fl == Stem then filterTableByPattern relPattern cleaned else cleaned
  when (fl == Stem) $ putStrLn $ "First filtering done, " ++ (show $ tSize cleaned') ++ " tokens."
  om <- analyseFDOmorfi cleaned'
  putStrLn $ "Omorfi analysis done, " ++ (show $ tSize om) ++ " tokens."
  let noError = clearErrors om
  putStrLn $ "Errors removed, " ++ (show $ tSize noError) ++ " tokens."
  let om' = filterByValTable (any getKnown) noError
  putStrLn $ "Unknown stems thrown away, " ++ (show $ tSize om') ++ " tokens."
  when (fl == Clean) (do
                      let om'' = takeWords om'
                      putStrLn $ "Wordforms found, " ++ (show $ tSize om'') ++ " tokens."
                      saveTable om'' savefn
                      )
  when (fl /= Clean) (do
                      let uncompounded = splitCompounds om'
                      putStrLn $ "Compounds split, " ++ (show $ length uncompounded) ++ " tokens."
                      let (verbs,notverbs) = splitVerbs uncompounded
                      putStrLn $ "Verbs split, " ++ (show $ length verbs) ++ " verbs."
                      stemmedverbs <- stemVerbs verbs
                      putStrLn $ "Verbs stemmed, " ++ (show $ length stemmedverbs) ++ " verbs."
                      let stemmed = takeStems stemmedverbs <> takeStems notverbs
                      putStrLn $ "Stemming done, " ++ (show $ tSize stemmed) ++ " tokens."
                      let filteredStemmed | fl == Stem = filterTable filterTokenRelevant stemmed
                                          | otherwise = stemmed
                      when (fl == Stem) $ putStrLn $ "Relevance determined, " ++ (show $ tSize filteredStemmed) ++ " tokens."
                      saveTable filteredStemmed savefn
                     )



main :: IO ()
main = do
  args <- getArgs
  (flags, fns) <- compileOptions args
  when (length fns < 1) (error "No files specified")
  when (length flags > 1) (error "Choose either -s, -o or -c")
  progVar <- initializeProgVar fns
  let filterAction | Stem `elem` flags = stemFDFile Stem
                   | Omorfi `elem` flags = stemFDFile Omorfi
                   | Clean `elem` flags = stemFDFile Clean
                   | otherwise = filterFDFile filterTokenRelevant cleanupWord
      runOneFile fn = do
        filterAction fn
        incrementProgVar progVar
        printWithProgVal printProgress progVar >>= putStrLn
  mapM_ runOneFile fns
  return ()
