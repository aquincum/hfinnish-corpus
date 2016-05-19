-- |The modules for segmenting tokens and analyzing their harmonic properties
module Hanalyze.Vowels (
  -- * Types
  HarmonyV(..), HarmonyW(..), Suffixing(..),

  -- * Basic harmonicity functions
  
  harmonyV, harmonyVM, harmonyVP, harmonyVPM, onlyVowels, harmonicity, fullHarmonic,
  suffixIt, wordHarmonies, abbreviateBFN, abbreviateBFNs

  ) where

import qualified Hanalyze.Token as T
import Hanalyze.Token (Token)
import Hanalyze.Phoneme
import Data.Maybe

-- |Harmony value of a vowel (Front, Neutral, Back), [i,e] are neutral
data HarmonyV = Front | Neutral | Back | HarmonyError deriving (Show, Eq)
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
harmonyVM :: Char -> Maybe HarmonyV
harmonyVM c
  | c `elem` "aou" = Just Back
  | c `elem` "ei" = Just Neutral
  | c `elem` "äöy" = Just Front
  | otherwise = Nothing

-- |Instead of Maybe, returns HarmonyError in case there is a problem
harmonyV :: Char -> HarmonyV
harmonyV = (fromMaybe HarmonyError) . harmonyVM
  
-- |Determines the harmony value of a vowel /phoneme/
harmonyVPM :: Phoneme -> Maybe HarmonyV
harmonyVPM p  
  | c `elem` ["a","aa","o","oo","u","uu", "ai", "oi", "ui", "au", "ou", "uo"] = Just Back
  | c `elem` ["e","ee","i","ii","ei","ie", "eu", "iu", "ey", "iy"] = Just Neutral -- ey, iy??? Not that frequent anyway. But should check out. ISK: they don't show up in 1st syllable anyway, except for leyh-
  | c `elem` ["ä", "ää", "ö", "öö", "y", "yy", "äi", "öi", "yi", "äy", "öy", "yö"] = Just Front
  | otherwise = Nothing
 where
   c = phonemeName p

-- |Instead of Maybe, returns HarmonyError in case there is a problem
harmonyVP :: Phoneme -> HarmonyV
harmonyVP = (fromMaybe HarmonyError) . harmonyVPM


-- |Returns the vowel harmonicities in a phoneme list as a list
wordHarmonies :: [Phoneme] -> [HarmonyV]
wordHarmonies [] = []
wordHarmonies (h:t) = case harmonyVPM h of
  Nothing -> wordHarmonies t
  Just harm -> harm:wordHarmonies t

-- |Simple abbreviation for shorter printing
abbreviateBFN :: HarmonyV -> Char
abbreviateBFN h = case h of
  Back -> 'B'
  Neutral -> 'N'
  Front -> 'F'
  HarmonyError -> '!'

-- |Simple abbreviated word form for a word
abbreviateBFNs :: [HarmonyV] -> String
abbreviateBFNs = map abbreviateBFN
  

                
-- |return a 'String' with the vowels only
onlyVowels :: Token -> Token
onlyVowels = T.filter (isJust . harmonyVM)


-- |Tells whether a token contains vowels exclusively in the given 'HarmonyV' category.
--
-- >>> fullHarmonic "atamo" Back
-- True
-- >>> fullHarmonic "ela" Neutral
-- False
fullHarmonic :: Token -> HarmonyV -> Bool
fullHarmonic str harm = T.foldl (\acc x -> harmonyVM x `elem` [Just harm, Nothing] && acc) True str

-- |Determines the 'HarmonyW' category of a word.
harmonicity :: Token -> HarmonyW
harmonicity t = case T.uncons t of -- x:xs
  Just (x, xs) -> let sofar = harmonicity xs in case harmonyVM x of
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
digraph "ai" = True
digraph "oi" = True
digraph "ui" = True
digraph "öi" = True
digraph "yi" = True
digraph "äi" = True
digraph "au" = True
digraph "ou" = True
digraph "iu" = True
digraph "eu" = True
digraph "ey" = True
digraph "iy" = True
digraph "öy" = True
digraph "äy" = True
digraph "uo" = True
digraph "yö" = True
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
