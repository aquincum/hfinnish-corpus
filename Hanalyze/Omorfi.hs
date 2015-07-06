{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings, DoAndIfThenElse, CPP, RecordWildCards #-}
-- |This module is responsible for communication with Omorfi, a stemmer for Finnish 
module Hanalyze.Omorfi where

import qualified Hanalyze.Token as T
import qualified Data.Text as Txt
import qualified Data.Text.IO as TxtIO
import Hanalyze.Token (Token)
import Hanalyze.FreqDist
import Hanalyze.Progress
import System.IO
import System.Process
import System.Process.Internals (ProcessHandle(..), ProcessHandle__(..))
import System.Posix.Terminal
import System.Posix.IO
import System.Posix.Signals
import System.Timeout
import Control.Monad
import qualified Data.Map as Map
import Data.Monoid
import Text.Parsec
import Control.Concurrent
import Control.Arrow

#ifndef CABAL_INSTALL
-- Need this for GHCi
import qualified Data.Text as Txt
instance (Monad m) => Stream Txt.Text m Char where
    uncons = return . Txt.uncons
#endif


-- DEBUG: import System.IO.Unsafe

-- |A data structure that is responsible to handle communication between Omorfi and
-- Hanalyze. For now, @omorfi-interactive.sh@ will be invoked and its input and
-- output handles managed within this module.
data OmorfiPipe = OmorfiPipe {
  oIn :: Handle,
  oOut :: Handle,
  oPh :: ProcessHandle
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
  getKnown :: Bool,
  getOtherInfo :: OtherInfo,
  getWeight :: Double,
  getFrequency :: Freq
  } | OmorfiInfoError Token
                deriving (Eq,Show)

-- |A Frequency Distribution with Omorfi stemmed information
data OmorfiFD = OmorfiFD { getFDMap :: Map.Map Token [OmorfiInfo] } deriving Eq

-- |So that 'Hanalyze.FreqDist.writeTable' can be used on 'OmorfiFD'!
instance Table OmorfiFD [OmorfiInfo] where
  tEmpty = OmorfiFD Map.empty
  tConstruct = const OmorfiFD
  tGetMap = getFDMap
  tPrintfun _ (mkey, mval) = mconcat $ map printOInfo mval
    where
      printOInfo oi = case oi of
        OmorfiInfo pos stem kn othi weight freq ->
          mconcat [mkey, "\t",
                   stem, "\t",
                   if kn then "known" else "unknown", "\t",
                   T.pack $ show  pos, "\t",
                   if othi == NoOI then "--" else getOIToken othi, "\t",
                   T.pack $ show weight, "\t",
                   T.pack $ show freq,
                   "\n"]
        OmorfiInfoError err -> mconcat [mkey, "\t", err]
  
-- |Initializes an Omorfi connection by starting the interactive process
initOmorfi :: IO OmorfiPipe
initOmorfi = do
  (fd1,fd2) <- openPseudoTerminal
  master <- fdToHandle fd1
  slave <- fdToHandle fd2
  let oproc = (proc "omorfi-interactive.sh" []){
        std_in = UseHandle slave,
        std_out = UseHandle slave,
        std_err = UseHandle slave,
        create_group = True
        }
  (_, _, _, ph) <- createProcess oproc
  hSetEncoding master utf8
  return $ OmorfiPipe master master ph

-- |Given an Omorfi connection, analyzes a token to its possible analyses
getOmorfiAnalysis :: OmorfiPipe -> Token -> IO [OmorfiInfo]
getOmorfiAnalysis (OmorfiPipe inh outh ph) tok = do
  T.hPutStrLn inh tok
  hFlush inh
  cont <- timeout (1000*1000*1000) (getUntilEmptyLine outh)
  case cont of
    Nothing -> do
      putStrLn $ "Problem with " ++ T.unpack tok
      return []
    Just anal -> 
      case parse parseToken "omorfi" anal of
        Left e -> putStrLn ("Omorfi parsing error -- " ++ show e ++ "\n" ++ Txt.unpack anal) >> return []
        Right (tok', ofis) -> return ofis


-- |Internal function that reads input until
-- it reaches an empty line before the Omorfi prompt ">"
getUntilEmptyLine :: Handle -> IO Txt.Text
getUntilEmptyLine h = do
  line <- Txt.strip `liftM` TxtIO.hGetLine h
  case line of
    "" -> return "\n"
    l -> getUntilEmptyLine h >>= \x -> return $ Txt.intercalate "\n" [l,x]
      
-- |Tries to wrap up an Omorfi connection. 
closeOmorfi :: OmorfiPipe -> IO ()
closeOmorfi (OmorfiPipe inh _ ph) = do
  hClose inh
  terminateProcessGroup ph
  terminateProcess ph
  waitForProcess ph
  return ()
 where
  terminateProcessGroup p = do
    let (ProcessHandle pmvar _) = p
    ph_ <- readMVar pmvar
    case ph_ of
      OpenHandle pid -> signalProcessGroup sigTERM pid
      otherwise -> return ()


  

-- |Analyses a 'FreqDist' with interactive omorfi
analyseFDOmorfi :: FreqDist -> IO OmorfiFD
analyseFDOmorfi fd = do
  omorfi <- initOmorfi
  let tokenfreqs = tToList fd
  progVar <- initializeProgVar tokenfreqs
  let runToken (tok, freq) = if T.length tok == 0 then return (T.pack "", [OmorfiInfoError $ T.pack "Empty token"]) else
        do
        oanal <- getOmorfiAnalysis omorfi tok
        let freqPerAnalysis = if null oanal then freq else freq `div` length oanal
            oanalfreqs = map (\x -> case x of
                                 OmorfiInfo{..} -> x { getFrequency = freqPerAnalysis }
                                 OmorfiInfoError x -> OmorfiInfoError x
                             ) oanal
        incrementProgVar progVar
        printWithProgVal printEveryPercent progVar
        return (tok,oanalfreqs)
  analysed <- mapM runToken tokenfreqs
  closeOmorfi omorfi
  return $ tFromList analysed
      
clearErrors :: OmorfiFD -> OmorfiFD
clearErrors o = OmorfiFD $ Map.map removeErrors (getFDMap o)
  where
    removeErrors ois = filter isError ois
    isError oi = case oi of
      OmorfiInfo {..} -> True
      OmorfiInfoError _ -> False

-- |Loads a file that was created by @omorfi-analyse.sh@ and returns its
-- contents as an 'OmorfiFD'
loadOmorfiFile :: FilePath -> IO OmorfiFD
loadOmorfiFile fn = do
  contents <- TxtIO.readFile fn
  case parse parseFile fn contents of
    Left e -> putStrLn ("Parse error: " ++ show e) >> return tEmpty
    Right omi -> return omi

-- |Stems a corpus: the input 'OmorfiFD' is converted to a 'FreqDist'
-- of stems,
getStems :: OmorfiFD -> FreqDist
getStems omfd =
  let
    tokenlist = tToList omfd
    stemOneLine ::  (Token, [OmorfiInfo]) -> [(Token, Freq)]
    --    stemOneLine oline@(tok, ois) = map (getStem &&& getFrequency) ois
    stemOneLine oline@(tok, ois) = concatMap (\oi ->
                                         let parts = T.split (=='#') (getStem oi)
                                         in
                                          zip parts (replicate (length parts) $ getFrequency oi)
                                          ) ois
    stemFreqs = concatMap stemOneLine tokenlist
    flattenedfd = splitListByTable id stemFreqs
  in
   flattenedfd



-- |Parser for an entire Omorfi analysis file
parseFile :: Parsec Txt.Text st OmorfiFD
parseFile =  do
--  toks <- sepEndBy parseToken (endOfLine>>endOfLine)
  toks <- many parseToken
  eof
  return $ tFromList toks
  
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
  maybePrompt
  tok <- firstLine
  eol
  analyses <- many1 $ analysisLine tok
  eol
  return (tok,analyses)
    where
      eol :: Parsec Txt.Text st ()
      sep :: Parsec Txt.Text st ()
      eol = (many (tab <|> char ' ') >> endOfLine >> return ()) <|> eof
      sep = skipMany1 (tab <|> char ' ')
      double :: Parsec Txt.Text st Double
      double = liftM (fst . head . reads) (many1 (oneOf "0123456789."))
      parseOneWord :: Parsec Txt.Text st Token
      parseOneWord =  T.pack `liftM` many1 (noneOf "\n\t+? ")
      maybePrompt :: Parsec Txt.Text st ()
      maybePrompt = void $ many (string "> ")
      firstLine :: Parsec Txt.Text st Token
      firstLine = parseOneWord
      analysisLine :: Token -> Parsec Txt.Text st OmorfiInfo
      analysisLine tok = do
        maybePrompt
        fullword <- parseOneWord
        sep
        stem <- parseOneWord
        known <- option True (T.pack `liftM` string "+?" >> return False)
--        let x = unsafePerformIO $ putStrLn $ "STEM (" ++ T.unpack stem ++ ")"
--        x `seq` sep
        sep
        readpos <- parseOneWord
        let pos = case readpos of
              "N" -> N
              "V" -> V
              _ -> Other
--        leftover <- (try (sep >> T.pack `liftM` many1 (noneOf "\n"))) <|>
  --                  (try eol >> T.pack `liftM` string "")
        leftover <- option (T.pack "") (sep>>T.pack `liftM` many1 (noneOf "\n\t"))
        weight <- option 0.0 (sep >> double)
        eol
        -- DEBUG: 
        -- DEBUG: if x `seq` fullword /= tok then
        if fullword /= tok then
          return $ OmorfiInfoError $ mconcat [tok, "/=", fullword,  ": first field of analysis must be the token."]
        else 
            return $ OmorfiInfo pos stem known (if leftover == "" then NoOI else OtherInfo leftover) weight 0

