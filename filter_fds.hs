module Main where

import qualified Data.Map as Map
import System.IO
import System.Environment
import qualified Control.Monad as M
import Control.Exception
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Process
import Hanalyze.Progress
import Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import Data.Char
import Data.Maybe (isNothing, isJust)

-- |In my dissertation, I'll be looking at C[i,e,ie]C[a,ä] forms and more generally C[i,e,ie]CV forms.
-- First try: C[i,e,ie,ei]C[a,ä] stems are relevant
--
-- Example:
--
-- >>> relevantStem (segment "aliaala") []
-- False
relevantStem :: [Phoneme] -- ^token recursively folded left-to-right
                -> [Phoneme] -- ^saved list of vowels so far
                -> Bool  -- ^the return value
relevantStem [] [v1,v2] = True
relevantStem [] _ = False
relevantStem (h:t) l
  | isNothing $ harmonyV $ head (phonemeName h) = relevantStem t l
relevantStem (h:t) [v1,v2] = (isNothing . harmonyV $ head (phonemeName h)) && relevantStem t [v1,v2]
relevantStem (h:t) [v1] = ((phonemeName h) `elem` ["a","aa","ä","ää"]) && relevantStem t [v1,h] 
relevantStem (h:t) [] = ((phonemeName h) `elem` ["e","i","ee","ii","ei","ie"]) && relevantStem t [h]

-- |Filter a token based on relevance 
filterTokenRelevant :: Token ->  Bool
filterTokenRelevant t = case segment finnishInventory t of
  Nothing -> False
  Just x -> relevantStem x []

-- |Cleaning up non-alphanumeric symbols. Could get more complicated
cleanupWord :: Token -> Token
cleanupWord = T.filter (`elem` "abcdefghijklmnopqrstuvwxyzäö")


main :: IO ()
main = do
  fns <- getArgs
  mapM_ (filterFDFile filterTokenRelevant cleanupWord) fns
  return ()
