module Tasks.FilterFDs (taskStem, taskStemFilter, taskClean, taskClassicFilter) where

import Control.Monad
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Progress
import Hanalyze.Phoneme
import Hanalyze.Pattern
import Hanalyze.Omorfi
import qualified Hanalyze.Token as T
import Data.Maybe (isNothing)
import Data.Monoid
import System.FilePath.Posix
import Tasks.Task

-- Option parsing:

data LocalTask = StemFilter | Stem | Clean | ClassicFilter deriving (Show, Eq)

taskStem :: Task
taskStem = Task
           (doTask Stem)
           (Just FileName)
           "stem"
           "Stems the corpus. Can work on more files. Goes through all of the words, cleans them, finds stems, and outputs to files prefixed with filtered3_. Old filter_fds -o"
  
taskStemFilter :: Task
taskStemFilter = Task
           (doTask StemFilter)
           (Just FileName)
           "stemfilter"
           "Stems the corpus and filters to CIC(C)A stems. Might be a bit deprecated. Can work on more files. Goes through all of the words, cleans them, finds stems, filters to CIC(C)A stems and outputs to files prefixed with filtered2_. Old filter_fds -s"
  
taskClean :: Task
taskClean = Task
           (doTask Clean)
           (Just FileName)
           "clean"
           "Just cleans the corpus to have a clean token word forms freqdist. Can work on more files. Goes through all of the words, cleans them, finds stems, and outputs to files prefixed with cleaned_. Old filter_fds -c"
  
taskClassicFilter :: Task
taskClassicFilter = Task
           (doTask ClassicFilter)
           (Just FileName)
           "classicfilter"
           "Deprecated. Simply cleans up and filters to CIC(C)A word forms. Does no stemming whatsoever. cleaned_. Old filter_fds (no flags)"


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
relevantStem [] [_,_] = True
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

stemFDFile :: LocalTask -- ^Input flag -- Stem or StemFilter or Clean
              -> FilePath -- ^Input file
              -> IO ()
stemFDFile fl fn = do
  let saveprefix | fl == StemFilter = "filtered2_"
                 | fl == Stem = "filtered3_"
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
  let cleaned' = if fl == StemFilter then filterTableByPattern relPattern cleaned else cleaned
  when (fl == StemFilter) $ putStrLn $ "First filtering done, " ++ (show $ tSize cleaned') ++ " tokens."
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
                      let filteredStemmed | fl == StemFilter = filterTable filterTokenRelevant stemmed
                                          | otherwise = stemmed
                      when (fl == StemFilter) $ putStrLn $ "Relevance determined, " ++ (show $ tSize filteredStemmed) ++ " tokens."
                      saveTable filteredStemmed savefn
                     )



doTask :: LocalTask -> [Flag] -> IO ()
doTask lt flags = do
  let fns =  map (\(FileName f) -> f) $ getAllFlags flags FileName
  when (length fns < 1) (error "No files given.")
  progVar <- initializeProgVar fns
  let filterAction | lt == ClassicFilter = filterFDFile filterTokenRelevant cleanupWord
                   | otherwise = stemFDFile lt
  let runOneFile fn = do
        filterAction fn
        incrementProgVar progVar
        printWithProgVal printProgress progVar >>= putStrLn
  mapM_ runOneFile fns
  return ()
