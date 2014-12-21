{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, OverloadedStrings #-}
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

newtype OmorfiPipe = OmorfiPipe { oIn :: Handle, oOut :: Handle}

data POS = N | V | Other deriving (Eq, Show)
data OtherInfo = NoOI | OtherInfo { getOIToken :: Token} deriving (Eq, Show)

data OmorfiInfo = OmorfiInfo {
  getPOS :: POS,
  getStem :: Token,
  getOtherInfo :: OtherInfo
  } | OmorfiInfoError Token
                deriving (Eq,Show)
                  
data OmorfiFD = OmorfiFD { getFDMap :: Map.Map Token [OmorfiInfo] } deriving Eq
type OParse = Parsec Token OmorfiFD

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
      

initOmorfi :: IO OmorfiPipe



loadOmorfiFile :: FilePath -> IO OmorfiFD
loadOmorfiFile fn = do
  contents <- TxtIO.readFile fn
  case parse parseFile fn contents of
    Left e -> putStrLn ("Parse error: " ++ show e) >> return tEmpty
    Right omi -> return omi


parseFile :: Parsec Txt.Text st OmorfiFD
parseFile =  do
--  toks <- sepEndBy parseToken (endOfLine>>endOfLine)
  toks <- many parseToken
  eof
  return $ OmorfiFD $ Map.fromList toks
  

parseToken :: Parsec Txt.Text st (Token,[OmorfiInfo])
parseToken = do
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

