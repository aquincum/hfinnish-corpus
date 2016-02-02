-- |The modules for segmenting tokens and analyzing their harmonic properties
module Hanalyze.Vowels (
  -- * Types
  HarmonyV(..), HarmonyW(..), Suffixing(..),

  -- * Basic harmonicity functions
  
  harmonyV, harmonyVP, onlyVowels, harmonicity, fullHarmonic,
  suffixIt, wordHarmonies, abbreviateBFN, abbreviateBFNs

  ) where

import qualified Hanalyze.Token as T
import Hanalyze.Token (Token)
import Hanalyze.Phoneme
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

-- |Determines the harmony value of a vowel /phoneme/
harmonyVP :: Phoneme -> Maybe HarmonyV
harmonyVP p  
  | c `elem` ["a","aa","o","oo","u","uu"] = Just Back
  | c `elem` ["e","ee","i","ii","ei","ie"] = Just Neutral
  | c `elem` ["ä", "ää", "ö", "öö", "y", "yy"] = Just Front
  | otherwise = Nothing
 where
   c = phonemeName p

-- |Returns the vowel harmonicities in a word as a list
wordHarmonies :: Token -> [HarmonyV]
wordHarmonies tok = case T.uncons tok of
  Nothing -> []
  Just (h, t) -> case harmonyV h of
    Nothing -> wordHarmonies t
    Just harm -> harm:wordHarmonies t

-- |Simple abbreviation for shorter printing
abbreviateBFN :: HarmonyV -> Char
abbreviateBFN h = case h of
  Back -> 'B'
  Neutral -> 'N'
  Front -> 'F'

-- |Simple abbreviated word form for a word
abbreviateBFNs :: [HarmonyV] -> String
abbreviateBFNs = map abbreviateBFN
  

                
-- |return a 'String' with the vowels only
onlyVowels :: Token -> Token
onlyVowels = T.filter (isJust . harmonyV)


-- |Tells whether a token contains vowels exclusively in the given 'HarmonyV' category.
--
-- >>> fullHarmonic "atamo" Back
-- True
-- >>> fullHarmonic "ela" Neutral
-- False
fullHarmonic :: Token -> HarmonyV -> Bool
fullHarmonic str harm = T.foldl (\acc x -> harmonyV x `elem` [Just harm, Nothing] && acc) True str

-- |Determines the 'HarmonyW' category of a word.
harmonicity :: Token -> HarmonyW
harmonicity t = case T.uncons t of -- x:xs
  Just (x, xs) -> let sofar = harmonicity xs in case harmonyV x of
    Just Back -> case sofar of
      AllFront -> FrontBack
      BackNeutral -> BackNeutral
      FrontNeutral -> FrontBack
      AllNeutral -> BackNeutral
      _ -> AllBack
    Just Front -> case sofar of
      AllBack -> FrontBack
      BackNeutral -> FrontBack
      FrontNeutral -> FrontNeutral
      AllNeutral -> FrontNeutral
      _ -> AllFront
    Just Neutral -> case sofar of
      AllBack -> BackNeutral
      BackNeutral -> BackNeutral
      FrontNeutral -> FrontNeutral
      AllFront -> FrontNeutral
      _ -> AllNeutral
    _ -> sofar
  _ -> Anything


-- |Determines whether the word is back or front suffixing given a 'HarmonyW' category
suffixIt :: HarmonyW -> Suffixing
suffixIt FrontBack = LastVowel
suffixIt w = if w `elem` [AllFront, FrontNeutral, AllNeutral] then FrontSuffixes else BackSuffixes

-- |Tests whether two letters constitute one phoneme
digraph :: String -> Bool
digraph "ie" = True
digraph "ei" = True
digraph [x,y] = x==y
digraph _ = False
{-
-- |Breaks down a token into a list of segments
segment :: Token -> [Segment]
segment tok = case T.uncons2 tok of
  Nothing -> case T.uncons tok of
    Nothing -> []
    Just (c, _) -> [[c]]
  Just (h, f, t) -> if digraph [h,f] then [h,f] : segment t else [h] : segment (T.cons f t)


-}
