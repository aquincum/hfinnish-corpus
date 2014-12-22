module Main where

import qualified Data.Map as Map
import System.IO
import System.IO.Temp
import System.Environment
import System.Process
import Control.Monad 
--import Control.Exception
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Process
import Hanalyze.Progress
import Hanalyze.Phoneme
import qualified Hanalyze.Token as T
--import Data.Char
--import Data.Maybe (isNothing, isJust)

main :: IO ()
main = do
  fd <- liftM head getArgs >>= readFreqDist
  withTempFile "stemming" "nofreq." $ \tmpFile h -> do
    sequence $ map (T.hPutStrLn h) (Map.keys $ getMap fd)
    runCommand $ "omorfi_analyse " ++ tmpFile
  return ()
