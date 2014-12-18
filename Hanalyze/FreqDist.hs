{-# LANGUAGE BangPatterns, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies  #-}
-- |The module for importing corpora into frequency distributions.
module Hanalyze.FreqDist
       (
         -- * Types
         Token, Table(..), Annotation,
         FreqDist(..), SummaryTable(..), AnnotatedFreqDist(..),

         -- * Simple lifted Map functions
         fdEmpty, fdKeys, sdEmpty, afdEmpty,


         
         -- * Reading and saving FreqDists
         countFreqs, multiReadCountFreqs, readCountFreqs,
         saveFreqDist, readFreqDist, writeTable,
         writeCountFreqs, writeSummaryTable,

         -- * Manipulating FreqDists
         cleanupFD, filterFD, filterByFreqFD, filterFDFile, splitByFD,
         sumFD, summarizeFD, annotateFD


         )
       where

import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import System.Environment
import Control.Applicative
import Data.Monoid
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.UTF8 as BUTF8
import qualified System.IO.MMap as MMap
import qualified Data.List as List 
import qualified Hanalyze.Token as T
import Hanalyze.Token (Token)
import Data.List.Split (splitOn)
import System.FilePath.Posix


-- |Tables that can be written out
class Eq t => Table t val | t -> val where
  tEmpty :: t
  tConstruct :: t -> Map.Map Token val -> t
  tGetMap :: t -> Map.Map Token val
  tPrintfun :: t -> (Token, val) -> Token
--  eq :: t -> t -> Bool
  

-- |The frequency distribution: it is a map where keys are types and the values show the token frequency.
data FreqDist = FreqDist {getMap :: !(Map.Map Token Int)} deriving (Eq,Show)

{-
instance NFData FreqDist where
  rnf fd = rnf $ getMap fd
-}
instance Monoid FreqDist where
  mempty = fdEmpty
  -- |Appending two 'FreqDist's by adding up the values in keys
  mappend !left !right =  let innermap = Map.unionWith (+) (left `seq` getMap left) (right `seq` getMap right) in
    FreqDist $ innermap

instance Table FreqDist Int where
  tEmpty = fdEmpty
  tConstruct = \_ -> FreqDist
  tGetMap = getMap
  tPrintfun _ (mkey, mval) = mconcat [mkey, T.pack "\t", T.pack $ show mval]

-- |Annotation is just a 'Data.Text.Text', therefore a 'Token'
type Annotation = Token

-- |Annotated frequency distribution: a key (a token) links to:
--
-- 1. annotated information (first value in the tuple) of the type 'Annotation'
-- 2. token frequency, as with 'FreqDist'
--
data AnnotatedFreqDist = AnnotatedFreqDist {getAFDMap :: Map.Map Token (Annotation, Int)} deriving (Eq, Show)

instance Table AnnotatedFreqDist (Annotation, Int) where
  tEmpty = afdEmpty
  tConstruct = \_ -> AnnotatedFreqDist
  tGetMap = getAFDMap
  tPrintfun _ (mkey, mval) = mconcat [mkey, T.pack "\t", T.pack $ show $ fst mval, T.pack "\t", T.pack $ show $ snd mval]
  

-- |Summary table of frequency distributions, where the token is probably some
-- summing factor. The value is a tuple, where the first value is, as usual,
-- token frequency, and the second value is the type frequency.
data SummaryTable = SummaryTable {getSTMap :: Map.Map Token (Int, Int)} deriving (Eq, Show)

instance Table SummaryTable (Int, Int) where
  tEmpty = sdEmpty
  tConstruct = \_ -> SummaryTable
  tGetMap = getSTMap         
  tPrintfun _ (mkey, mval) = mconcat [mkey, T.pack "\t", T.pack $ show $ fst mval, T.pack "\t", T.pack $ show $ snd mval]

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
  let (w1,w2) =  BUTF8.break (== '\t') line
      txt2 = case T.decodeFromUTF $ BUTF8.drop 1 w2 of
        Left err -> T.pack "0"
        Right x -> x
      in
  case T.decodeFromUTF w1 of
    Left err -> (T.pack "UnicodeError", txt2)
    Right txt1 -> (txt1, txt2)


-- |Converts a Text with the frequency info to an integer. Implemented to
-- test different techniques
readFrequency :: Token -> Int
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


-- |Generic 'Table' writer. The individual functions 'writeCountFreqs' and 'writeSummaryTable' are preserved for a bit.
writeTable :: (Table a x) => a -> Handle -> IO () 
writeTable fd _ | fd == tEmpty = return ()
writeTable fd handle =
  let ((mkey, mval), mfd2) = Map.deleteFindMax (tGetMap fd) in
  do
    T.hPutStrLn handle $ tPrintfun fd (mkey,mval)
    writeTable (tConstruct fd mfd2) handle


  

-- |Recursively writes out the token frequencies in a 'FreqDist', ordered in theory, but needs fixing.
writeCountFreqs :: FreqDist -> Handle -> IO ()
writeCountFreqs  = writeTable

-- |Recursively writes out the token frequencies in a 'SummaryTable', ordered in theory, but needs fixing.
writeSummaryTable :: SummaryTable -> Handle -> IO ()
writeSummaryTable = writeTable


{-
-- |Recursively writes out the token frequencies in a 'SummaryTable', ordered in theory, but needs fixing.
writeSummaryTable :: SummaryTable -> Handle -> IO ()
writeSummaryTable fd _
  | fd == sdEmpty = return ()
writeSummaryTable fd handle =
  let ((mkey,mval),mfd2) = Map.deleteFindMax (getSTMap fd) in do
    T.hPutStrLn handle $ mconcat [mkey, T.pack "\t", T.pack $ show $ fst mval, T.pack "\t", T.pack $ show $ snd mval]
    writeSummaryTable (SummaryTable mfd2) handle


-}
{-saveFreqDist' fd fn = do
  let map = getMap fd
      l = unlines . 
  -}                        


-- |Saves a 'FreqDist' to a file, using 'writeCountFreqs' inside.
saveFreqDist :: FreqDist -> FilePath -> IO ()
saveFreqDist fd fn = putStrLn ("Saving " ++ fn) >>
                     openFile fn WriteMode >>= \handle ->
                     writeCountFreqs fd handle >>
                     hClose handle

-- |Cleans up a 'FreqDist' using the cleanup 'Token' -> 'Token' function that converts the filthy
-- string to the cleaned up string
cleanupFD :: (Token -> Token) -> FreqDist -> FreqDist
cleanupFD cleanup fd = let map = getMap fd
                           accumulate acc key val = Map.unionWith (+) acc $ Map.singleton (cleanup key) val
                           cleaned = Map.foldlWithKey' accumulate Map.empty map
                       in
                         FreqDist cleaned


-- |Filters a FreqDist based on a filtering function that has the
-- type 'Token' -> 'Bool'
filterFD :: (Token -> Bool) -> FreqDist -> FreqDist
filterFD f fd = FreqDist $ Map.filterWithKey expfilt $ getMap fd
  where
    expfilt tok _ = f tok

-- |Filters a FreqDist based on a filtering function that has the
-- type 'Int' -> 'Bool', as it filters on frequency data
filterByFreqFD :: (Int -> Bool) -> FreqDist -> FreqDist
filterByFreqFD f fd = FreqDist $ Map.filter f  $ getMap fd

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
  saveFreqDist (filterFD f. cleanupFD cleanup $ fd) savefn


-- |Splits a 'FreqDist' into a summary 'FreqDist's based on a partitioning
-- function.
splitByFD :: (Eq a, Show a) =>
             (Token -> a) -- ^partitioning function
             -> FreqDist  -- ^input 'FreqDist'
             -> FreqDist  -- ^summary 'FreqDist'
splitByFD func fd =
  let map = getMap fd
      retvals = List.map (\(x,y) -> (func x,y)) $ Map.toList map
      classes = List.nub $ List.map fst retvals
      sum classid = List.foldl' (\(x,y) (k,z) ->  y `seq` (x,if k == classid then y+z else y)) (T.pack $ show classid,0) retvals
  in
      FreqDist . Map.fromList $ List.map (sum) classes


-- |Similar to 'splitByFD' but creates a summary table with token *and*
-- type frequency.
summarizeFD :: (Eq a, Show a) =>
               (Token -> a) -- ^partitioning function
               -> FreqDist  -- ^input 'FreqDist'
               -> SummaryTable  -- ^summary table as 'SummaryTable'
summarizeFD func fd =
  let map = getMap fd
      retvals = List.map (\(x,y) -> (func x,y)) $ Map.toList map
      classes = List.nub $ List.map fst retvals
      sum classid = List.foldl' (\(x,(y,typ)) (k,z) -> y `seq` typ `seq` (x,(if k == classid then y+z else y,if k == classid then typ + 1 else  typ))) (T.pack $ show classid,(0,0)) retvals
  in
   SummaryTable . Map.fromList $ List.map (sum) classes

-- |Annotates a 'FreqDist' with a list of filter functions that are paired
-- with annotation labels.
annotateFD :: [(Annotation, Token -> Bool)] -- ^a list of partitioning functions paired with labels. If the function returns Just x, the token will be annotated, otherwise the given function will not annotate the token.
              -> FreqDist -- ^input 'FreqDist'
              -> AnnotatedFreqDist -- ^output 'AnnotatedFreqDist'
annotateFD funlist fd =
  let innerlist = Map.toList $ getMap fd
      getAnnotation :: (Annotation, Token -> Bool) -> Token -> Annotation
      getAnnotation (ann,fun) tok = case fun tok of
        False -> T.pack ""
        True -> ann
      getAllAnnotations :: [(Annotation, Token -> Bool)] -> Token -> Annotation
      getAllAnnotations annfuns tok = let anns = List.map (\f -> getAnnotation f tok) annfuns
                                      in
                                       mconcat $ List.intercalate [T.pack ", "] [anns]
      annmap = List.map (\(x,y) -> (x, (getAllAnnotations funlist x, y))) innerlist
  in
   AnnotatedFreqDist $ Map.fromList annmap

      
-- |Simple summing function: returns the grand total frequency.
sumFD :: FreqDist -> Int
sumFD fd = Map.foldl' (+) 0 $ getMap fd


  
