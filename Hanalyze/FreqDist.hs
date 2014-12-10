{-# LANGUAGE BangPatterns #-}
-- |The module for importing corpora into frequency distributions.
module Hanalyze.FreqDist
       (
         -- * Types
         Token, Segment, FreqDist(..), 

         -- * Simple lifted Map function
         fdEmpty, fdKeys,


         
         -- * Reading and saving FreqDists
         multiReadCountFreqs, readCountFreqs,
         saveFreqDist, readFreqDist, writeCountFreqs,

         -- * Manipulating FreqDists
         cleanupFD, filterFD, filterFDFile, splitByFD, sumFD


         )
       where

import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import System.Environment
import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString as B
import qualified System.IO.MMap as MMap
import qualified Data.List as List 



-- |As imported from the corpus
type Token = T.Text
-- |Words are segmented to phonemes, where diphthongs, long vowels and geminates are treated as one Segment.
type Segment = String
-- |The frequency distribution: it is a map where keys are types and the values show the token frequency.
data FreqDist = FreqDist {getMap :: !(Map.Map Token Int)} deriving (Eq,Show)

instance NFData FreqDist where
  rnf fd = rnf $ getMap fd

instance Monoid FreqDist where
  mempty = fdEmpty
  -- |Appending two 'FreqDist's by adding up the values in keys
  mappend !left !right =  let innermap = Map.unionWith (+) (left `seq` getMap left) (right `seq` getMap right) in
    left `deepseq` right `deepseq` innermap `deepseq` FreqDist $ innermap


-- |The empty FreqDist map.
fdEmpty :: FreqDist
fdEmpty = FreqDist Map.empty

-- |Returns the keys as the list
fdKeys :: FreqDist -> [Token]
fdKeys = Map.keys . getMap


-- |Loads a corpus file into a list of tokens.
loadFile :: FilePath -> IO [Token]
loadFile fn = do
  let contents = T.toLower <$> TIO.readFile fn -- Text
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
readFreqDistLine :: BUTF8.ByteString -> (T.Text, T.Text)
readFreqDistLine line =
  let (w1,w2) =  BUTF8.break (== '\t') line
      txt2 = case TE.decodeUtf8' $ BUTF8.drop 1 w2 of
        Left err -> T.pack "0"
        Right x -> x
      in
  case TE.decodeUtf8' w1 of
    Left err -> (T.pack "UnicodeError", txt2)
    Right txt1 -> (txt1, txt2)


-- |Convert a Text with the frequency info to an integer. Implemented to
-- test different techniques
readFrequency :: T.Text -> Int
readFrequency s = case TR.decimal s of
  Left _ -> -1
  Right (num,rem) -> num
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


-- |Recursively write out the token frequencies in a 'FreqDist', ordered in theory, but needs fixing.
writeCountFreqs :: FreqDist -> Handle -> IO ()
writeCountFreqs fd _
  | fd == fdEmpty = return ()
writeCountFreqs fd handle =
  let ((mkey,mval),mfd2) = Map.deleteFindMax (getMap fd) in do
    TIO.hPutStrLn handle $ T.concat [mkey, T.pack "\t", T.pack $ show mval]
    writeCountFreqs (FreqDist mfd2) handle


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

-- |Cleans up a 'FreqDist' using the cleanup function that converts the filty
-- string to the cleaned up string
cleanupFD :: (T.Text -> T.Text) -> FreqDist -> FreqDist
cleanupFD cleanup fd = let map = getMap fd
                           accumulate acc key val = Map.unionWith (+) acc $ Map.singleton (cleanup key) val
                           cleaned = Map.foldlWithKey' accumulate Map.empty map
                       in
                         FreqDist cleaned


-- |Filter a FreqDist based on a filtering function that has the
-- type 'Token -> Bool'
filterFD :: (Token -> Bool) -> FreqDist -> FreqDist
filterFD f fd = FreqDist $ Map.filterWithKey expfilt $ getMap fd
  where
    expfilt tok _ = f tok


-- |Filter a Frequency Distribution file with a filter function and
-- a cleanup function
filterFDFile :: (Token -> Bool) -- ^The filtering function
                -> (Token -> Token) -- ^The cleanup function
                -> FilePath -- ^The file name
                -> IO ()
filterFDFile f cleanup fn = do
  fd <- readFreqDist fn
  saveFreqDist (filterFD f. cleanupFD cleanup $ fd) ("filtered_"++fn)


-- |Split a 'FreqDist' into a summary 'FreqDist's based on a partitioning
-- function.
splitByFD :: (Eq x, Show x) =>
             (Token -> x) -- ^partitioning function
             -> FreqDist  -- ^input 'FreqDist'
             -> FreqDist  -- ^summary 'FreqDist'
splitByFD func fd =
  let map = getMap fd
      retvals = List.map (\(x,y) -> (func x,y)) $ Map.toList map
      classes = List.nub $ List.map fst retvals
      sum classid = List.foldl' (\(x,y) (k,z) -> (x,if k == classid then y+z else y)) (T.pack $ show classid,0) retvals
  in
      FreqDist . Map.fromList $ List.map (sum) classes

sumFD :: FreqDist -> Int
sumFD fd = Map.foldl' (+) 0 $ getMap fd


  
