module Tasks.FilterFDs (taskStem, taskStemFilter, taskClean, taskClassicFilter, taskFilterByPattern, taskFilterForStems) where

import           Control.Monad
import qualified Data.Map as Map
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
--
data LocalTask = StemFilter
               | Stem
               | Clean
               | ClassicFilter
               | FilterByPattern
               | FilterForStems FreqDist
               deriving (Show, Eq)

isFilterForStems :: LocalTask -> Bool
isFilterForStems lt = case lt of
  FilterForStems _ -> True
  _ -> False


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

taskFilterForStems :: Task
taskFilterForStems = Task
           doFilterForStemsTask
           (Just FileName)
           "filterforstems"
           "Filters a frequency distribution to include only words whose stem is found in a list of stems. This list of stems is given as a freqdist, and has to be given as the first filename argument."


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
  | isNothing $ harmonyVM $ head (phonemeName h) = relevantStem t l
relevantStem (h:t) [v1,v2] = (isNothing . harmonyVM $ head (phonemeName h)) && relevantStem t [v1,v2]
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

matchStems :: LocalTask -> OmorfiSFD -> OmorfiSFD
matchStems (FilterForStems stemfd) tofilter =
  let
    stemmap :: Map.Map Token Freq -- ^a map of stems to filter for
    stemmap = tGetMap stemfd  
    tofilterStems :: [Token] -- ^The stems for each token in the fd
    tofilterStems = map (getStem . snd) tofilter
    tofilterMapList :: [(Token,(Token, OmorfiInfo))] -- ^A list with stem--(token,ois) elements
    tofilterMapList = zip tofilterStems tofilter
    tofilterMapListSingletons :: [(Token,[(Token, OmorfiInfo)])] -- ^Same, but we need a singleton inside for the map+intersection to work
    tofilterMapListSingletons = map (\(st, (tok,oi)) -> (st, [(tok, oi)])) tofilterMapList
    tofilterMap :: Map.Map Token [(Token, OmorfiInfo)] -- ^The map, with stems as keys, and lists of appeared token-oi's with values
    tofilterMap = Map.fromListWith (++) tofilterMapListSingletons
    onlyContained :: Map.Map Token [(Token, OmorfiInfo)] -- ^This was our goal! Now we have a map like above, with only the tokens whose stems are contained in stemmap
    onlyContained = Map.intersection tofilterMap stemmap
    onlyContainedList :: [(Token, [(Token, OmorfiInfo)])] -- ^List back and we just need to concat the 'snd's
    onlyContainedList = Map.toList onlyContained
  in
   concatMap snd onlyContainedList

matchStems _ _ = error "matchStems only works with FilterForStems"


stemFDFile :: PhonemicInventory -- ^The inventory to work on
              -> LocalTask -- ^Input flag -- Stem or StemFilter or Clean or FilterForStems
              -> FilePath -- ^Input file
              -> IO ()
stemFDFile inv fl fn = do
  let saveprefix | fl == StemFilter = "filtered2_"
                 | fl == Stem = "filtered3_"
                 | fl == Clean = "cleaned_"
                 | isFilterForStems fl = "filteredFS_"
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
  tableToSave <- case fl of
    Clean -> do
      let om'' = takeWords om'
      putStrLn $ "Wordforms found, " ++ (show $ tSize om'') ++ " tokens."
      return om''
    _ -> do
      uncompounded <- case fl of
            FilterForStems _ -> return $ omorfiToSFD om'
            _ -> do
              let sc = splitCompounds om'
              putStrLn $ "Compounds split, " ++ (show $ length sc) ++ " tokens."
              return sc
      let (verbs,notverbs) = splitVerbs uncompounded
      putStrLn $ "Verbs split, " ++ (show $ length verbs) ++ " verbs."
      stemmedverbs <- stemVerbs verbs
      putStrLn $ "Verbs stemmed, " ++ (show $ length stemmedverbs) ++ " verbs."
      let stemmed = takeStems stemmedverbs <> takeStems notverbs
      putStrLn $ "Stemming done, " ++ (show $ tSize stemmed) ++ " tokens."
      let filteredStemmed | fl == StemFilter = filterTable (filterTokenRelevant inv) stemmed
                          | isFilterForStems fl = takeTokens $ matchStems fl (stemmedverbs <> notverbs)
                          | otherwise = stemmed
      when (fl == StemFilter) $ putStrLn $ "Relevance determined, " ++ (show $ tSize filteredStemmed) ++ " tokens."
      when (isFilterForStems fl) $ putStrLn $ "Stems matched, " ++ (show $ tSize filteredStemmed) ++ " tokens."
      return filteredStemmed
  saveTable tableToSave savefn

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

doFilterForStemsTask :: [Flag] -> IO ()
doFilterForStemsTask flags = do
  let allfnargs = map (\(FileName f) -> f) $ getAllFlags flags FileName
  progVar <- initializeProgVar allfnargs
  when (length allfnargs < 2) (error "You have to give at least two files:\nhanalyze filterforstems <stemfd> [fds...]")
  stemfd <- readFreqDist $ head allfnargs
  let runOneFile fn = do
        stemFDFile (theInventory flags) (FilterForStems stemfd) fn
        incrementProgVar progVar
        printWithProgVal printProgress progVar >>= putStrLn
  mapM_ runOneFile (tail allfnargs)
  return ()
  
      
