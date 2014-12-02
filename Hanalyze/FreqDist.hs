{-# LANGUAGE BangPatterns #-}
-- |The module for importing corpora into frequency distributions.
module Hanalyze.FreqDist
       (
         -- * Types
         Token, Segment, FreqDist(..), fdEmpty,

         -- * Reading and saving FreqDists
         multiReadCountFreqs, readCountFreqs,
         saveFreqDist, readFreqDist)
       where

import qualified Data.Map.Strict as Map
import qualified Control.Monad as M
import System.IO
import System.Environment
import Control.Applicative
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Lazy.Read as TR
import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as BUTF8
import qualified Data.ByteString.Lazy as B
import qualified System.IO.Posix.MMap.Lazy as MMap

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
  let (w1,w2) = {-# SCC rFDLwords #-} BUTF8.break (\c -> c == '\t') line
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
  Left _ -> (-1)
  Right (num,rem) -> num
-- ancient:readFrequency = (read . T.unpack)


-- |Reads a saved FreqDist file
readFreqDist :: FilePath -> IO FreqDist
readFreqDist fp = do
--  ls <- {-# SCC lineing #-} fmap BUTF8.lines ({-# SCC reading #-}MMap.mmapFileByteString fp Nothing)
  ls <- {-# SCC lineing #-} fmap BUTF8.lines ({-# SCC reading #-}MMap.unsafeMMapFile fp)
--  ls <- {-# SCC lineing #-} fmap BUTF8.lines ({-# SCC reading #-}B.readFile fp)
  let pairs = {-# SCC mapping #-} map readFreqDistLine ls
      stringmap = {-# SCC mapbuilding #-} Map.fromList pairs
      fd = FreqDist $ {-# SCC fdbuilding #-} Map.map readFrequency stringmap
  putStrLn (T.unpack $ TE.decodeUtf8 (ls!!2))
  putStrLn (T.unpack $ snd (pairs!!2))
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

