{-# LANGUAGE BangPatterns #-}
-- |The module for importing corpora into frequency distributions.
module Hanalyze.FreqDist
       (
         -- * Types
         Token, Segment, FreqDist(..),

         -- * Reading and saving FreqDists
         fdEmpty,
         multiReadCountFreqs, readCountFreqs,
         saveFreqDist, readFreqDist)
       where

import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import System.Environment
import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.DeepSeq
import Control.Exception

-- |As imported from the corpus
type Token = T.Text
-- |Words are segmented to phonemes, where diphthongs, long vowels and geminates are treated as one Segment.
type Segment = String
-- |The frequency distribution: it is a map where keys are types and the values show the token frequency.
data FreqDist = FreqDist {getMap :: !(Map.Map Token Integer)} deriving (Eq,Show)

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
readFreqDistLine :: T.Text -> (T.Text, T.Text)
readFreqDistLine line =
  let (w1:w2:t) = {-# SCC rFDLwords #-} T.words line in
  ({-# SCC rFDLpairb #-} (w1,w2))

-- |Reads a saved FreqDist file
readFreqDist :: FilePath -> IO FreqDist
readFreqDist fp = do
  ls <- {-# SCC lineing #-} fmap T.lines ({-# SCC reading #-}TIO.readFile fp)
  let pairs = {-# SCC mapping #-} map readFreqDistLine ls
      stringmap = {-# SCC mapbuilding #-} Map.fromList pairs
      fd = FreqDist $ {-# SCC fdbuilding #-} Map.map (\s -> (read $ T.unpack s) :: Integer) stringmap
  return ( {-# SCC returning #-} fd)


-- |Recursively write out the token frequencies in a 'FreqDist', ordered in theory, but needs fixing.
-- Initiate with a Nothing Handle
writeCountFreqs :: FreqDist -> FilePath -> Maybe Handle -> IO ()
writeCountFreqs fd _ _ 
  | fd == fdEmpty = return ()
writeCountFreqs fd fn Nothing = do
  handle <- openFile fn WriteMode
  writeCountFreqs fd fn $ Just handle
  hClose handle
  return ()
writeCountFreqs fd fn (Just handle) = let ((mkey,mval),mfd2) = Map.deleteFindMax (getMap fd) in do
    TIO.hPutStrLn handle $ T.concat [mkey, T.pack "\t", T.pack $ show mval]
    writeCountFreqs (FreqDist mfd2) fn $ Just handle

-- |Saves a 'FreqDist' to a file, using 'writeCountFreqs' inside.
saveFreqDist :: FreqDist -> FilePath -> IO ()
saveFreqDist fd fn = writeCountFreqs fd fn Nothing

