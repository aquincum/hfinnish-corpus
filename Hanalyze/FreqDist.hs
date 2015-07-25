{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies  #-}
-- |The module for importing corpora into frequency distributions.
module Hanalyze.FreqDist
       (
         -- * Types
         Token, Freq, Table(..), Annotation,
         FreqDist(..), SummaryTable(..), AnnotatedFreqDist(..),

         -- * Simple lifted Map functions
         fdEmpty, fdKeys, sdEmpty, afdEmpty,

         -- * Reading and saving FreqDists
         -- ** Generalized for Tables
         writeTable, saveTable,
         -- ** Specifically for raw FreqDists
         countFreqs, multiReadCountFreqs, readCountFreqs,
         readFreqDist,

         -- * Manipulating FreqDists
         -- ** Generalized for Tables
         filterTable, filterByValTable,
         cleanupTable, sumTable,
         splitByTable, splitListByTable,

         -- ** Specifically for raw FreqDists
         filterFDFile,
         -- ** Specifically for AnnotatedFreqDists
         dropAnnotation, filterWithAnnotation,
         -- ** Creating Tables from raw FreqDists
         summarizeFD, annotateFD


         )
       where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Exception
import qualified Control.Monad as M
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.List as List
import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Hanalyze.Token (Token)
import qualified Hanalyze.Token as T
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import qualified System.IO.MMap as MMap


-- |Tables that can be written out
class Eq t => Table t val  | t -> val where
  -- |The empty table
  tEmpty :: t
  -- |The constructor of the table based on a prototype to
  -- know what kind of table should be created
  tConstruct :: t -- ^The prototype table
             -> Map.Map Token val
             -> t
  -- |The unwrapper of the inner map
  tGetMap :: t -> Map.Map Token val
  -- |A function that produces a printable tab-separated line
  tPrintfun :: t -> (Token,val) -> Token
  -- |Wrapper for @toList . tGetMap@
  tToList :: t -> [(Token,val)]
  tToList = Map.toList . tGetMap
  -- |Wrapper for @tConstruct tEmpty (fromList)@
  tFromList :: [(Token,val)] -> t
  tFromList l =
    tConstruct tEmpty
               (Map.fromList l)
  -- |A map over the keys of the table
  tMap :: (Token -> Token) -> t -> t
  tMap fun tab = tConstruct tab $ Map.mapKeys fun $ tGetMap tab
  tSize :: t -> Int
  tSize = length . tToList
--  eq :: t -> t -> Bool

-- |Type synonym for frequency counts.
type Freq = Int 
instance Monoid Freq where
  mappend = (+)
  mempty = 0

-- |The frequency distribution: it is a map where keys are types and the values show the token frequency.
data FreqDist =
  FreqDist {getMap :: !(Map.Map Token Freq)}
  deriving (Eq,Show)

{-
instance NFData FreqDist where
  rnf fd = rnf $ getMap fd
-}
instance Monoid FreqDist where
  mempty = fdEmpty
  -- |Appending two 'FreqDist's by adding up the values in keys
  mappend !left !right =
    let innermap =
          Map.unionWith (+)
                        (left `seq` getMap left)
                        (right `seq` getMap right)
    in FreqDist innermap


instance Table FreqDist Freq where
  tEmpty = fdEmpty
  tConstruct = const FreqDist
  tGetMap = getMap
  tPrintfun _ (mkey,mval) =
    mconcat [mkey,T.pack "\t",T.pack $ show mval]

{-
instance (Table t v, Num v) => Monoid t where
  mempty = tEmpty
  mappend !left !right =
    let innermap =
          Map.unionWith (+)
                        (left `seq` tGetMap left)
                        (right `seq` tGetMap right)
    in tConstruct left innermap
-}

-- |Annotation is just a 'Data.Text.Text', therefore a 'Token'
type Annotation = Token

-- |Annotated frequency distribution: a key (a token) links to:
--
-- 1. annotated information (first value in the tuple) of the type 'Annotation'
-- 2. token frequency, as with 'FreqDist'
--
data AnnotatedFreqDist = AnnotatedFreqDist {getAFDMap :: Map.Map Token (Annotation, Freq)} deriving (Eq, Show)

instance Table AnnotatedFreqDist (Annotation, Freq) where
  tEmpty = afdEmpty
  tConstruct = const AnnotatedFreqDist
  tGetMap = getAFDMap
  tPrintfun _ (mkey, mval) = mconcat [mkey, T.pack "\t", T.pack $ show $ fst mval, T.pack "\t", T.pack $ show $ snd mval]


-- |Summary table of frequency distributions, where the token is probably some
-- summing factor. The value is a tuple, where the first value is, as usual,
-- token frequency, and the second value is the type frequency.
data SummaryTable = SummaryTable {getSTMap :: Map.Map Token (Freq, Freq)} deriving (Eq, Show)

instance Table SummaryTable (Freq, Freq) where
  tEmpty = sdEmpty
  tConstruct = const SummaryTable
  tGetMap = getSTMap
  tPrintfun _ (mkey, mval) = mconcat [mkey, T.pack "\t", T.pack $ show $ fst mval, T.pack "\t", T.pack $ show $ snd mval]

{-
data AnyTable = AnyTable {getATMap :: Map.Map Token [Float]} deriving (Eq, Show)

instance Table AnyTable [Float] where
  tEmpty = AnyTable Map.empty
  tConstruct = const AnyTable
  tGetMap = getATMap
  tPrintfun _ (mkey, mval) = mconcat [mkey, T.pack "\t",
                                      mconcat $ mconcat (map (\n -> [T.pack $ show n, T.pack "\t"]) mval)]
  -}

-- |The empty 'FreqDist' map.
fdEmpty :: FreqDist
fdEmpty = FreqDist Map.empty

-- |The empty 'SummaryTable' map.
sdEmpty :: SummaryTable
sdEmpty = SummaryTable Map.empty

-- |The empty 'AnnotatedFreqDist' map.
afdEmpty :: AnnotatedFreqDist
afdEmpty = AnnotatedFreqDist Map.empty

-- |Returns the keys as the list
fdKeys :: FreqDist -> [Token]
fdKeys = Map.keys . getMap


-- |Loads a corpus file into a list of tokens.
loadFile :: FilePath -> IO [Token]
loadFile fn = do
  let contents = T.toLower <$> T.readFile fn -- Text
      tokentxts = fmap T.words contents
  tokentxts

-- |Updates a 'FreqDist' with a token: adds +1 if the token exists as a key
-- or inserts a key with a value of 1 if this is not the case.o
addPlusToken :: FreqDist -> Token -> FreqDist
addPlusToken fd tok = FreqDist $ Map.insertWith (+) tok 1 (getMap fd)

-- |Creates a 'FreqDist' out of a set of tokens.
countFreqs :: [Token] -> FreqDist
countFreqs = foldl addPlusToken fdEmpty

-- |Reads the token frequencies to a 'FreqDist' map from a given file
readCountFreqs :: FilePath -> IO FreqDist
readCountFreqs fn = fmap countFreqs (loadFile fn)

-- |Reads frequency distributions from a list of files. First, it calls 'readCountFreq' on files
-- and then merges these distributions by adding the frequencies together.
multiReadCountFreqs :: [FilePath] -> IO FreqDist
multiReadCountFreqs fns = do
  fds <- sequence $ fmap readCountFreqs fns -- [FreqDist] <- IO [FreqDist]
  return $ mconcat fds

-- |Inside function to read in the first 2 words in a line to a pair
readFreqDistLine :: BUTF8.ByteString -> (Token, Token)
readFreqDistLine line =
  let (w1,w2) = BUTF8.break (== '\t') line
      txt2 =
        case T.decodeFromUTF $
             BUTF8.drop 1 w2 of
          Left err -> T.pack "0"
          Right x -> x
  in case T.decodeFromUTF w1 of
       Left err -> (T.pack "UnicodeError",txt2)
       Right txt1 -> (txt1,txt2)


-- |Converts a Text with the frequency info to an integer. Implemented to
-- test different techniques
readFrequency :: Token -> Freq
readFrequency s = case T.decimal s of
  Left _ -> -1
  Right num -> num
-- ancient:readFrequency = (read . T.unpack)


-- |Reads a saved FreqDist file
readFreqDist :: FilePath -> IO FreqDist
readFreqDist fp = do
  ls <- fmap BUTF8.lines (MMap.mmapFileByteString fp Nothing)
--  ls <- MMap.unsafeMMapFile fp)
--  ls <- B.readFile fp)
  let pairs =  map readFreqDistLine ls
      stringmap =  Map.fromList pairs
      fd = FreqDist $  Map.map readFrequency stringmap
  putStrLn $ "Loading " ++ fp
  return fd


-- |Generic 'Table' writer.
writeTable :: (Table a x) => a -> Handle -> IO ()
writeTable fd _ | fd == tEmpty = return ()
writeTable fd handle =
  let ((mkey, mval), mfd2) = Map.deleteFindMax (tGetMap fd) in
  do
    T.hPutStrLn handle $ tPrintfun fd (mkey,mval)
    writeTable (tConstruct fd mfd2) handle

-- |Saves a 'Table' to a file, using 'writeTable' inside.
-- for backwards compatibility
saveTable :: (Table a x) => a -> FilePath -> IO ()
saveTable fd fn = putStrLn ("Saving " ++ fn) >>
                  openFile fn WriteMode >>= \handle ->
                  writeTable fd handle >>
                  hClose handle

-- |Cleans up a 'Table' using the cleanup 'Token' -> 'Token' function that converts the filthy
-- string to the cleaned up string
cleanupTable :: (Table t v, Monoid v)  => (Token -> Token) -> t -> t
cleanupTable cleanup fd = let map = tGetMap fd
                              accumulate acc key val = Map.unionWith mappend acc $ Map.singleton (cleanup key) val
                              cleaned = Map.foldlWithKey' accumulate Map.empty map
                       in
                         tConstruct fd cleaned


-- |Filters a Table based on a filtering function that has the
-- type 'Token' -> 'Bool'
filterTable :: Table a x => (Token -> Bool) -> a -> a
filterTable f fd = tConstruct fd $ Map.filterWithKey expfilt $ tGetMap fd
  where
    expfilt tok _ = f tok

-- |Filters a Table based on a filtering function over the values
filterByValTable :: Table a x => (x -> Bool) -> a -> a
filterByValTable f fd = tConstruct fd $ Map.filter f  $ tGetMap fd


-- |Filters a Frequency Distribution file with a filter function and
-- a cleanup function
filterFDFile :: (Token -> Bool) -- ^The filtering function
                -> (Token -> Token) -- ^The cleanup function
                -> FilePath -- ^The file name
                -> IO ()
filterFDFile f cleanup fn = do
  let saveprefix = "filtered_"
      (dirname,fname) = splitFileName fn
      savefn = dirname </> (saveprefix ++ fname)
  fd <- readFreqDist fn
  saveTable (filterTable f. cleanupTable cleanup $ fd) savefn


-- |Splits a 'Table' into a summary 'Table' based on a partitioning
-- function.
splitByTable :: (Eq a, Show a, Table t v, Monoid v) =>
                (Token -> a) -- ^partitioning function
                -> t  -- ^input 'Table'
                -> t  -- ^summary 'Table'
splitByTable func fd = splitListByTable func (tToList fd)

-- |Splits a list of ('Token', value) pairs into a summary 'Table'
-- based on a partitioning function.
splitListByTable :: (Eq a, Show a, Table t v, Monoid v) =>
                    (Token -> a) -- ^partitioning function
                    -> [(Token, v)]  -- ^input list
                    -> t  -- ^summary 'Table'
splitListByTable func list =
  let retvals = List.map (first func) list
      classes = List.nub $ List.map fst retvals
      sum classid = List.foldl' (\(x,y) (k,z) ->  y `seq` (x,if k == classid then y `mappend` z else y)) (T.pack $ show classid,mempty) retvals
  in
      tFromList $ List.map sum classes


-- |Similar to 'splitByTable' but creates a summary table with token *and*
-- type frequency.
summarizeFD :: (Eq a, Show a) =>
               (Token -> a) -- ^partitioning function
               -> FreqDist  -- ^input 'FreqDist'
               -> SummaryTable  -- ^summary table as 'SummaryTable'
summarizeFD func fd =
  let map = getMap fd
      retvals = List.map (first func) $ Map.toList map
      classes = List.nub $ List.map fst retvals
      sum classid = List.foldl' (\(x,(y,typ)) (k,z) -> y `seq` typ `seq` (x,(if k == classid then y+z else y,if k == classid then typ + 1 else  typ))) (T.pack $ show classid,(0,0)) retvals
  in
   SummaryTable . Map.fromList $ List.map sum classes

-- |Annotates a 'FreqDist' with a list of filter functions that are paired
-- with annotation labels.
annotateFD :: [(Annotation, Token -> Bool)] -- ^a list of partitioning functions paired with labels. If the function returns Just x, the token will be annotated, otherwise the given function will not annotate the token.
              -> FreqDist -- ^input 'FreqDist'
              -> AnnotatedFreqDist -- ^output 'AnnotatedFreqDist'
annotateFD funlist fd =
  let innerlist = Map.toList $ getMap fd
      getAnnotation :: (Annotation, Token -> Bool) -> Token -> Annotation
      getAnnotation (ann,fun) tok = if fun tok then ann else T.pack ""
      getAllAnnotations :: [(Annotation, Token -> Bool)] -> Token -> Annotation
      getAllAnnotations annfuns tok = let anns = List.map (`getAnnotation` tok) annfuns
                                      in
                                       mconcat $ List.intercalate [T.pack ", "] [anns]
      annmap = List.map (\(x,y) -> (x, (getAllAnnotations funlist x, y))) innerlist
  in
   AnnotatedFreqDist $ Map.fromList annmap

-- |Drops the annotation  from an annotated freqdist, creating a plain 'FreqDist'.
dropAnnotation :: AnnotatedFreqDist -> FreqDist
dropAnnotation annfd = FreqDist $ Map.map (\(a,f) -> f) (getAFDMap annfd)

-- |Filters an 'AnnotatedFreqDist' based on its annotation
filterWithAnnotation :: (Annotation -> Bool) ->AnnotatedFreqDist -> AnnotatedFreqDist
filterWithAnnotation pred annfd = AnnotatedFreqDist $ Map.filter (\(a,_) -> pred a) (getAFDMap annfd)

-- |Simple summing function: returns the grand total frequency.
sumTable :: (Table t v, Monoid v) => t -> v
sumTable fd = Map.foldl' mappend mempty $ tGetMap fd
