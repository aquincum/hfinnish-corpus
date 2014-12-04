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
import qualified Data.Text as T
import Data.Char
import Data.Maybe (isNothing, isJust)

-- |In my dissertation, I'll be looking at C[i,e,ie]C[a,ä] forms and more generally C[i,e,ie]CV forms.
-- First try: C[i,e,ie,ei]C[a,ä] stems are relevant
--
-- Example:
--
-- >>> relevantStem (segment "aliaala") []
-- False
relevantStem :: [Segment] -- ^token recursively folded left-to-right
                -> [Segment] -- ^saved list of vowels so far
                -> Bool  -- ^the return value
relevantStem [] [v1,v2] = True
relevantStem [] _ = False
relevantStem (h:t) l
  | isNothing $ harmonyV $ head h = relevantStem t l
relevantStem (h:t) [v1,v2] = if isJust $ harmonyV $ head h then False else relevantStem t [v1,v2]
relevantStem (h:t) [v1] = if h `elem` ["a","aa","ä","ää"] then relevantStem t [v1,h] else False
relevantStem (h:t) [] = if h `elem` ["e","i","ee","ii","ei","ie"] then relevantStem t [h] else False

-- |Filter a token based on relevance 
filterTokenRelevant :: Token ->  Bool
filterTokenRelevant t = (relevantStem . segment) (T.unpack t) []

-- |Cleaning up non-alphanumeric symbols. Could get more complicated
cleanupWord :: Token -> Token
cleanupWord = T.filter isAlphaNum


main :: IO ()
main = do
  fns <- getArgs
  sequence $ map (filterFDFile filterTokenRelevant cleanupWord) fns
  return ()
