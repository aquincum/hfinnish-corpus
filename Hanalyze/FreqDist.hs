-- |The module for importing corpora into frequency distributions.
module Hanalyze.FreqDist
       (Token, Segment, FreqDist, multiReadCountFreqs, saveCountFreqs, readCountFreqs)
       where

import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import System.Environment
import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- |As imported from the corpus
type Token = String
-- |Words are segmented to phonemes, where diphthongs, long vowels and geminates are treated as one Segment.
type Segment = String
-- |The frequency distribution: it is a map where keys are types and the values show the token frequency.
type FreqDist = Map.Map Token Integer

-- |The empty FreqDist map.
fdEmpty :: FreqDist
fdEmpty = Map.empty

-- |Loads a corpus file into a list of tokens.
loadFile :: FilePath -> IO [Token]
loadFile fn = do
  let contents = fmap T.toLower $ TIO.readFile fn -- Text
      tokentxts = fmap T.words contents
      tokens = fmap (map T.unpack) tokentxts
  tokens

-- |Updates a 'FreqDist' with a token: adds +1 if the token exists as a key
-- or inserts a key with a value of 1 if this is not the case.o
addPlusToken :: FreqDist -> Token -> FreqDist
addPlusToken fd tok = Map.insertWith (+) tok 1 fd

-- |Creates a 'FreqDist' out of a set of tokens.
countFreqs :: [Token] -> FreqDist
countFreqs = foldl addPlusToken fdEmpty

-- |Reads the token frequencies to a 'FreqDist' map from a given file
readCountFreqs :: FilePath -> IO FreqDist
readCountFreqs fn = fmap countFreqs (loadFile fn)

-- |Reads frequency distributions from a list of files. First, it calls 'readCountFreq' on files
-- and then merges these distributions by adding the frequencies together.
--
-- First, we read in the files to separate FreqDists. This is (fmap readCountFreqs fns),
-- which is a ['FreqDist']. Now we make a fold over this. The initial accumulator is pure
-- Map.empty :: IO FreqDist, and the folding function merges the accumulator with the 
-- new FreqDist with unionWith: if a key is found in both maps, they are summed over.
-- Inside the lambda: Map.unionWith (+) takes 2 maps, therefore it is used in an applicative
-- manner, as x and y are IO monads. Or something like this.
multiReadCountFreqs :: [FilePath] -> IO FreqDist
multiReadCountFreqs fns = foldl (\x y -> Map.unionWith (+) <$> x <*> y) (pure Map.empty) (fmap readCountFreqs fns)

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
writeCountFreqs fd fn (Just handle) = let ((mkey,mval),fd2) = Map.deleteFindMax fd in do
    hPutStrLn handle (mkey ++ "\t" ++ show mval)
    writeCountFreqs fd2 fn $ Just handle

-- |Saves a 'FreqDist' to a file, using 'writeCountFreqs' inside.
saveCountFreqs :: FreqDist -> FilePath -> IO ()
saveCountFreqs fd fn = writeCountFreqs fd fn Nothing

