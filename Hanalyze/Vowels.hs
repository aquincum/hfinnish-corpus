-- |The modules for segmenting tokens and analyzing their harmonic properties
module Hanalyze.Vowels (
  -- * Types
  HarmonyV(..), HarmonyW(..), Suffixing(..),

  -- * Basic harmonicity functions
  
  harmonyV, onlyVowels, harmonicity, fullHarmonic,
  suffixIt, 

  -- * Segmenting 

  segment,
  ) where

import Hanalyze.FreqDist (Token, Segment)
import qualified Data.Text as T
import Data.Maybe

-- |Harmony value of a vowel (Front, Neutral, Back), [i,e] are neutral
data HarmonyV = Front | Neutral | Back deriving (Show, Eq)
-- |Harmonicity of a word, with only the information relevant for suffixation
data HarmonyW = Anything -- ^Only consonants, indeterminable
              | AllFront -- ^Only front non-neutral vowels
              | AllBack -- ^Only back vowels
              | BackNeutral -- ^Back vowel(s) followed by neutral vowel(s)
              | FrontNeutral -- ^Front vowel(s) followed by neutral vowel(s)
              | FrontBack -- ^Disharmonic stem, containing front and back non-neutral vowels
              | AllNeutral -- ^Only front neutral vowels
              deriving (Show, Eq)
-- |Whether a word is front suffixing or back suffixing
data Suffixing = BackSuffixes | FrontSuffixes | LastVowel deriving (Show, Eq)


-- |Determines the harmony value of a vowel /character/
harmonyV :: Char -> Maybe HarmonyV
harmonyV c
  | c `elem` "aou" = Just Back
  | c `elem` "ei" = Just Neutral
  | c `elem` "äöy" = Just Front
  | otherwise = Nothing

-- |return a 'String' with the vowels only
onlyVowels :: T.Text -> T.Text
onlyVowels = T.filter (isJust . harmonyV)


-- |Tells whether a token contains vowels exclusively in the given 'HarmonyV' category.
--
-- >>> fullHarmonic "atamo" Back
-- True
-- >>> fullHarmonic "ela" Neutral
-- False
fullHarmonic :: String -> HarmonyV -> Bool
fullHarmonic str harm = foldl (\acc x -> harmonyV x `elem` [Just harm, Nothing] && acc) True str

-- |Determines the 'HarmonyW' category of a word.
harmonicity :: String -> HarmonyW
harmonicity "" = Anything
harmonicity (x:xs)
  | harmonyV x == Just Back = case sofar of AllFront -> FrontBack
                                            BackNeutral -> BackNeutral
                                            FrontNeutral -> FrontBack
                                            AllNeutral -> BackNeutral
                                            _ -> AllBack
  | harmonyV x == Just Front = case sofar of AllBack -> FrontBack
                                             BackNeutral -> FrontBack
                                             FrontNeutral -> FrontNeutral
                                             AllNeutral -> FrontNeutral
                                             _ -> AllFront
  | harmonyV x == Just Neutral = case sofar of AllBack -> BackNeutral
                                               BackNeutral -> BackNeutral
                                               FrontNeutral -> FrontNeutral
                                               AllFront -> FrontNeutral
                                               _ -> AllNeutral
  | otherwise = sofar
  where sofar = harmonicity xs

-- |Determines whether the word is back or front suffixing given a 'HarmonyW' category
suffixIt :: HarmonyW -> Suffixing
suffixIt FrontBack = LastVowel
suffixIt w = if w `elem` [AllFront, FrontNeutral, AllNeutral] then FrontSuffixes else BackSuffixes

-- |Tests whether two letters constitute one phoneme
digraph :: Segment -> Bool
digraph "ie" = True
digraph "ei" = True
digraph [x,y] = x==y
digraph _ = False

-- |Breaks down a token into a list of segments
segment :: String -> [Segment]
segment "" = []
segment [v] = [[v]]
segment (h:f:t) = if digraph [h,f] then [h,f] : segment t else [h] : segment (f:t)


