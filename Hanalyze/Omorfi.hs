{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings, DoAndIfThenElse #-}
-- |This module is responsible for communication with Omorfi, a stemmer for Finnish 
module Hanalyze.Omorfi where

import qualified Hanalyze.Token as T
import qualified Data.Text as Txt
import qualified Data.Text.IO as TxtIO
import Hanalyze.Token (Token)
import Hanalyze.FreqDist
import System.IO
import System.Process
import Control.Monad
import qualified Data.Map as Map
import Data.Monoid
import Text.Parsec
import Control.Concurrent


-- |Status of an 'OmorfiPipe': can be open or closed
data OmorfiPipeStatus = OPSOpen | OPSClosed

-- |A data structure that is responsible to handle communication between Omorfi and
-- Hanalyze. For now, @omorfi-interactive.sh@ will be invoked and its input and
-- output handles managed within this module.
data OmorfiPipe = OmorfiPipe {
  oIn :: Handle,
  oOut :: Handle,
  oPh :: ProcessHandle,
  oStatus :: OmorfiPipeStatus
  }

-- |Part-of-speech information given by Omorfi -- curently not too much
-- sophistication is needed
data POS = N | V | Other deriving (Eq, Show)

-- |Morphological information given by Omorfi which will not be very important
-- here but it'll be saved
data OtherInfo = NoOI -- ^No extra information given
               | OtherInfo { getOIToken :: Token}  -- ^contains the extra morphological information as a 'Token'
               deriving (Eq, Show)

-- |A data structure representing the information given by Omorfi for a token:
-- contains POS information, the stem itself and additional morphological
-- information
data OmorfiInfo = OmorfiInfo {
  getPOS :: POS,
  getStem :: Token,
  getOtherInfo :: OtherInfo
  } | OmorfiInfoError Token
                deriving (Eq,Show)

-- |A Frequency Distribution with Omorfi stemmed information
data OmorfiFD = OmorfiFD { getFDMap :: Map.Map Token [OmorfiInfo] } deriving Eq

-- |So that 'Hanalyze.FreqDist.writeTable' can be used on 'OmorfiFD'!
instance Table OmorfiFD [OmorfiInfo] where
  tEmpty = OmorfiFD $ Map.empty
  tConstruct = \_ -> OmorfiFD
  tGetMap = getFDMap
  tPrintfun _ (mkey, mval) = mconcat $ map printOInfo mval
    where
      printOInfo oi = case oi of
        OmorfiInfo pos stem othi -> mconcat [mkey, "\t",
                                             stem, "\t",
                                             T.pack $ show $ pos, "\t",
                                             if othi == NoOI then "--" else getOIToken $ othi,
                                             "\n"]
        OmorfiInfoError err -> mconcat [mkey, "\t", err]

-- |Initializes an Omorfi connection by starting the interactive process
initOmorfi :: IO OmorfiPipe
initOmorfi = do
  let oproc = (proc "omorfi-interactive.sh" []){
        std_in = CreatePipe,
        std_out = CreatePipe,
        std_err = UseHandle stderr
        }
  (Just inh, Just outh, _, ph) <- createProcess oproc
  hSetBuffering inh NoBuffering
  hSetBuffering outh NoBuffering
  return $ OmorfiPipe inh outh ph OPSOpen

-- |Given an Omorfi connection, analyzes a token to its possible analyses
getOmorfiAnalysis :: OmorfiPipe -> Token -> IO [OmorfiInfo]
getOmorfiAnalysis (OmorfiPipe _ _ _ OPSClosed) _ = error "Trying to read closed omorfi"
getOmorfiAnalysis (OmorfiPipe inh outh ph OPSOpen) tok = do
  T.hPutStrLn inh tok
  hFlush inh
  cont <- getUntilPrompt outh (Txt.pack "")
  case parse parseToken "omorfi" cont of
    Left e -> (putStrLn $ "Omorfi parsing error -- " ++ show e) >> return []
    Right (tok', ofis) -> return ofis


-- |Internal function which does not work yet. Reads input until
-- it reaches the Omorfi prompt ">"
getUntilPrompt :: Handle -> Txt.Text -> IO Txt.Text
getUntilPrompt h str = do
  ch <- hGetChar h
  putStrLn ("got " ++ [ch])
  case ch of
    '>' -> return $ Txt.reverse str
    c -> getUntilPrompt h (c `Txt.cons` str)
      
-- |Tries to wrap up an Omorfi connection. It returns the stats given by
-- Omorfi when quitting the program as a string.
closeOmorfi :: OmorfiPipe -> IO String
closeOmorfi (OmorfiPipe _ _ _ OPSClosed) = return "Already closed"
closeOmorfi (OmorfiPipe inh outh ph OPSOpen) = do
  hClose inh
  stats <-  hGetContents outh
  hClose outh
  return stats

-- |Loads a file that was created by @omorfi-analyse.sh@ and returns its
-- contents as an 'OmorfiFD'
loadOmorfiFile :: FilePath -> IO OmorfiFD
loadOmorfiFile fn = do
  contents <- TxtIO.readFile fn
  case parse parseFile fn contents of
    Left e -> putStrLn ("Parse error: " ++ show e) >> return tEmpty
    Right omi -> return omi


-- |Parser for an entire Omorfi analysis file
parseFile :: Parsec Txt.Text st OmorfiFD
parseFile =  do
--  toks <- sepEndBy parseToken (endOfLine>>endOfLine)
  toks <- many parseToken
  eof
  return $ OmorfiFD $ Map.fromList toks
  
-- |Parser for a token's results as given by Omorfi:
--
-- >>>
-- tokenname
-- tokenname stem POS ...
-- tokenname stem POS ...
-- tokenname stem POS ...
-- empty line
-- >>>
parseToken :: Parsec Txt.Text st (Token,[OmorfiInfo])
parseToken = do
  optional (string "> ")
  tok <- firstLine
  eol
  analyses <- many1 $ analysisLine tok
  eol
  eol
  return (tok,analyses)
    where
      eol :: Parsec Txt.Text st ()
      sep :: Parsec Txt.Text st ()
      eol = (many (tab <|> char ' ') >> endOfLine >> return ()) <|> eof
      sep = skipMany1 (tab <|> char ' ')
      parseOneWord :: Parsec Txt.Text st Token
      parseOneWord =  T.pack `liftM` many1 (noneOf "\n\t ")
      firstLine :: Parsec Txt.Text st Token
      firstLine = parseOneWord
      analysisLine :: Token -> Parsec Txt.Text st OmorfiInfo
      analysisLine tok = do
        fullword <- parseOneWord
        sep
        stem <- parseOneWord
        sep
        readpos <- parseOneWord
        let pos = case readpos of
              "N" -> N
              "V" -> V
              _ -> Other
--        leftover <- (try (sep >> T.pack `liftM` many1 (noneOf "\n"))) <|>
  --                  (try eol >> T.pack `liftM` string "")
        leftover <- option (T.pack "") (sep>>T.pack `liftM` many1 (noneOf "\n"))
        if fullword /= tok then
          return $ OmorfiInfoError $ mconcat [tok, "/=", fullword,  ": first field of analysis must be the token."]
        else 
            return $ OmorfiInfo pos stem (if leftover == "" then NoOI else OtherInfo leftover)

