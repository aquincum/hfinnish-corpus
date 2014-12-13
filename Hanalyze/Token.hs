-- |This module is for the definition of our 'Token' type: it is abstracted away so
-- that we can change its actual implementation to, say, a 'ByteString' etc., anytime!
module Hanalyze.Token (
  -- * Constructing, packing, unpacking
  Token, pack, unpack,

  -- * cons'ing
  cons, uncons, uncons2,

  -- * Other needed functions to lift
  --  for append, concat -- use Monoid functions
  words, toLower, filter,
  decodeFromUTF, decimal,

  -- * Folds, maps
  foldl,

  -- * IO functions
  readFile, hPutStrLn

  ) where

import qualified Data.ByteString.UTF8 as BUTF8
import qualified Data.ByteString as B
import qualified Data.Text as Txt
import qualified Data.Text.IO as TxtIO
import qualified Data.Text.Encoding as TxtE
import qualified Data.Text.Encoding.Error as TxtE.Error
import qualified Data.Text.Read as TxtR
import qualified Data.String as String
import Data.Monoid
import Control.DeepSeq
import Prelude hiding (concat, filter, readFile, hPutStrLn, words, foldl)
import Control.Monad
import System.IO hiding (readFile, hPutStrLn)

-- needed from Text: unpack, filter, pack, toLower, words, concat

-- |As imported from the corpus
newtype Token = Tok {getText :: Txt.Text} deriving (Eq,Ord)

-- |Need to write own instance, to return the inner string with 'show'.
instance Show Token where
  show = unpack

-- |Strings can be overloaded
instance String.IsString Token where
  fromString = pack

-- |For appending and concating, use 'Monoid'
instance Monoid Token where
  mempty = pack ""
  mappend = append
  mconcat = concat -- probably more powerful?

-- |Tokens can also be deepseq'd
instance NFData Token where
  rnf a = (unpack a) `seq` ()
  
-- |Get the string out of the 'Token'
unpack :: Token -> String
unpack = Txt.unpack . getText 

-- |Wrap a string into a the 'Token'
pack :: String -> Token
pack = Tok . Txt.pack

-- |Construct a 'Token' with a head 'Char', and a tail 'Token'
cons :: Char -> Token -> Token
cons ch tok = Tok . Txt.cons ch $ getText tok

-- |Deconstruct a 'Token' to its head and the tail. If the
-- 'Token' is empty, it returns 'Nothing'
uncons :: Token -> Maybe (Char, Token)
uncons tok = liftM (\a -> (fst a, (Tok . snd) a)) $ Txt.uncons $ getText tok

-- |Deconstruct a 'Token' to its head, the following char
-- and the tail. If the 'Token' is empty or has only one char,
-- it returns 'Nothing'
uncons2 :: Token -> Maybe (Char, Char, Token)
uncons2 tok = do
  (c1, tk2) <- uncons tok
  (c2, tk3) <- uncons tk2
  return (c1,c2,tk3)

-- |Appends two Tokens
append :: Token -> Token -> Token
append a b = Tok $ Txt.append (getText a) (getText b)

-- |Concatenates a list of tokens
concat :: [Token] -> Token
concat ts = Tok . Txt.concat $ map getText ts

-- |Separates a text to words
words :: Token -> [Token]
words t = map Tok $ Txt.words $ getText t

-- |Lowercases a token
toLower :: Token -> Token
toLower = Tok . Txt.toLower . getText

-- |Filters a token using a 'Char -> Bool' function
filter :: (Char -> Bool) -> Token -> Token
filter func t = Tok . Txt.filter func $ getText t

-- |Decodes from UTF8, returns either an error on the Left, or the
-- token on the RIght
decodeFromUTF :: B.ByteString -> Either TxtE.Error.UnicodeException Token
decodeFromUTF bs = Tok `liftM` TxtE.decodeUtf8' bs

-- |Reads a token containing a decimal integer and returns it on
-- the Right if parsable. The remainder is discarded.
decimal :: Token -> Either String Int
decimal t = fst `liftM` (TxtR.decimal . getText $ t)

-- |Reads in a file to a long token
readFile :: FilePath -> IO Token
readFile fn = Tok `liftM` TxtIO.readFile fn

-- |Outputs a token to a handle
hPutStrLn :: Handle -> Token -> IO ()
hPutStrLn h t = TxtIO.hPutStrLn h $ getText t

-- |Simple left fold on a token
foldl :: (a -> Char -> a) -> a -> Token -> a
foldl func init tok = Txt.foldl func init $ getText tok
