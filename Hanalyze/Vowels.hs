-- |The modules for segmenting tokens and analyzing their harmonic properties
module Hanalyze.Vowels (
  HarmonyV(..), HarmonyW(..), Suffixing(..), harmonyV, harmonicity, fullHarmonic,
  suffixIt, segment, relevantStem
  ) where

import Hanalyze.FreqDist (Token, Segment)

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
data Suffixing = BackSuffixes | FrontSuffixes deriving (Show, Eq)

-- |Determines the harmony value of a vowel /character/
harmonyV :: Char -> Maybe HarmonyV
harmonyV c 
  | c `elem` "aou" = Just Back
  | c `elem` "ei" = Just Neutral
  | c `elem` "äöy" = Just Front
  | otherwise = Nothing

-- |Tells whether a token contains vowels exclusively in the given 'HarmonyV' category.
--
-- >>> fullHarmonic "atamo" Back
-- True
-- >>> fullHarmonic "ela" Neutral
-- False
fullHarmonic :: Token -> HarmonyV -> Bool
fullHarmonic str harm = foldl (\acc x -> if harmonyV x `elem` [Just harm, Nothing] then (acc && True) else False) True str

-- |Determines the 'HarmonyW' category of a word.
harmonicity :: Token -> HarmonyW
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
                                               AllFront -> AllFront
                                               _ -> AllNeutral
  | otherwise = sofar
  where sofar = harmonicity xs

-- |Determines whether the word is back or front suffixing given a 'HarmonyW' category
suffixIt :: HarmonyW -> Suffixing
suffixIt w = if w `elem` [AllFront, FrontNeutral, AllNeutral] then FrontSuffixes else BackSuffixes

-- |Tests whether two letters constitute one phoneme
digraph :: Segment -> Bool
digraph "ie" = True
digraph "ei" = True
digraph [x,y] = if x==y then True else False
digraph _ = False

-- |Breaks down a token into a list of segments
segment :: Token -> [Segment]
segment "" = []
segment [v] = [[v]]
segment (h:f:t) = if digraph [h,f] then [[h,f]] ++ segment t else [[h]] ++ segment (f:t)

-- |In my dissertation, I'll be looking at C[i,e,ie]C[a,ä] forms and more generally C[i,e,ie]CV forms.
-- First try: C[i,e,ie,ei]C[a,ä] stems are relevant
--
-- Run as:
--
-- >>> relevantStem (segment "aliaala") []
-- False
relevantStem :: [Segment] -- ^token recursively folded left-to-right
                -> [Segment] -- ^saved list of vowels so far
                -> Bool  -- ^the return value
relevantStem [] [v1,v2] = True
relevantStem [] _ = False
relevantStem (h:t) l
  | harmonyV (h!!0) == Nothing = relevantStem t l
relevantStem (h:t) [v1,v2] = if harmonyV (h!!0) /= Nothing then False else relevantStem t [v1,v2]
relevantStem (h:t) [v1] = if h `elem` ["a","aa","ä","ää"] then relevantStem t [v1,h] else False
relevantStem (h:t) [] = if h `elem` ["e","i","ee","ii","ei","ie"] then relevantStem t [h] else False
