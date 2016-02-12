module Tasks.FilterFDs (taskStem, taskStemFilter, taskClean, taskClassicFilter, taskFilterByPattern) where

import           Control.Monad
import           Data.Maybe (isNothing)
import           Data.Monoid
import           Hanalyze.FreqDist
import           Hanalyze.Omorfi
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import           Hanalyze.Progress
import qualified Hanalyze.Token as T
import           Hanalyze.Vowels
import           System.FilePath.Posix
import           Tasks.Task

-- Option parsing:

data LocalTask = StemFilter | Stem | Clean | ClassicFilter | FilterByPattern deriving (Show, Eq)

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

taskFilterByPattern :: Task
taskFilterByPattern = Task
           (doTask FilterByPattern)
           (Just FileName)
           "filterbypattern"
           "Filter freqdists by pattern."


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
filterTokenRelevant :: PhonemicInventory -> Token -> Bool
filterTokenRelevant inv t = case segment inv t of
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

stemFDFile :: PhonemicInventory -- ^The inventory to work on
              -> LocalTask -- ^Input flag -- Stem or StemFilter or Clean
              -> FilePath -- ^Input file
              -> IO ()
stemFDFile inv fl fn = do
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
                      let filteredStemmed | fl == StemFilter = filterTable (filterTokenRelevant inv) stemmed
                                          | otherwise = stemmed
                      when (fl == StemFilter) $ putStrLn $ "Relevance determined, " ++ (show $ tSize filteredStemmed) ++ " tokens."
                      saveTable filteredStemmed savefn
                     )


parseFlagPattern :: [Flag] -> [Pattern]
parseFlagPattern fl = case getFlag fl FPattern of
  Nothing -> error "No pattern given."
  Just (FPattern pattstr) -> case readPattern (theInventory fl) (T.pack pattstr) of
    Nothing -> error "Wrong pattern."
    Just patt -> patt

doTask :: LocalTask -> [Flag] -> IO ()
doTask lt flags = do
  let fns =  map (\(FileName f) -> f) $ getAllFlags flags FileName
  when (length fns < 1) (error "No files given.")
  progVar <- initializeProgVar fns
  let filterAction | lt == ClassicFilter = filterFDFile (filterTokenRelevant (theInventory flags)) cleanupWord
                   | lt == FilterByPattern = filterFDFile (filterToken (theInventory flags) (parseFlagPattern flags)) cleanupWord
                   | otherwise = stemFDFile (theInventory flags) lt
  let runOneFile fn = do
        filterAction fn
        incrementProgVar progVar
        printWithProgVal printProgress progVar >>= putStrLn
  mapM_ runOneFile fns
  return ()
