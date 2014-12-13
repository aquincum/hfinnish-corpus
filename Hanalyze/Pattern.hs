module Hanalyze.Pattern (
  -- * Data type
  Pattern(..),

  -- * String conversions
  readPattern, writePattern,

  -- * Filtering
  filterWord, filterToken
  ) where

import Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import Hanalyze.Token (Token, pack, unpack)


-- |A pattern for filtering
data Pattern = P Phoneme -- ^Matches a given phoneme based on its label
             | Dot -- ^Matches one and one phoneme only
             | Star -- ^Matches 0 or more phonemes
             | Question -- ^Matches 0 or 1 phoneme
             | DotF FeatureBundle -- ^Matches a feature bundle
             | StarF FeatureBundle -- ^Matches 0 or more phonemes that match the feature bundle
             | QuestionF FeatureBundle -- ^Matches 0 or 1 phonemes that match the feature bundle

instance Show Pattern where
  show patt = writePattern [patt]

-- |Read in a pattern based on a phonemic inventory. Need a better parser; use with caution!!
readPattern :: PhonemicInventory -> Token -> Maybe [Pattern]
readPattern inv s = do
  let inv' = inv ++ [
        Phoneme "*" emptyBundle,
        Phoneme "?" emptyBundle,
        Phoneme "." emptyBundle
        ]
  seg <- segment inv' s
  let patt = map (\p -> case phonemeName p of
                     "*" -> Star
                     "." -> Dot
                     "?" -> Question
                     s -> P p
                 ) seg
  return patt
                     
-- |Output a pattern in a human-readable form
writePattern :: [Pattern] -> String
writePattern pat = foldl (++) [] (map write' pat)
  where
    write' x = case x of
      Star -> "*"
      Question -> "?"
      Dot -> "."
      P y -> phonemeName y
      DotF fb -> (foldl (\acc feat -> acc ++ (show feat) ++ ",") "[" $ getBundle fb) ++ "]"
      StarF fb -> (foldl (\acc feat -> acc ++ (show feat) ++ ",") "[" $ getBundle fb) ++ "]*"
      QuestionF fb -> (foldl (\acc feat -> acc ++ (show feat) ++ ",") "[" $ getBundle fb) ++ "]?" 

-- |Filter a word as a list of phonemes based on a pattern
filterWord :: [Phoneme] -> [Pattern] -> Bool
filterWord [] [] = True
filterWord ph [] = False
filterWord [] patt = case head patt of
  Question -> filterWord [] (tail patt)
  Star -> filterWord [] (tail patt)
  QuestionF _ -> filterWord [] (tail patt)
  StarF _ -> filterWord [] (tail patt)
  _ -> False
filterWord phall@(phtop:phrest) pattall@(pattop:pattrest) = case pattop of
  P phon | phonemeName phon == phonemeName phtop -> filterWord phrest pattrest
         | otherwise -> False
  DotF fb | subsetFB fb (featureBundle phtop) -> filterWord phrest pattrest
          | otherwise -> False
  Dot -> filterWord phrest pattrest
  Question | filterWord phrest pattrest -> True
           | filterWord phall pattrest -> True
           | otherwise -> False
  Star | filterWord phall pattrest -> True
       | filterWord phrest pattall -> True
       | otherwise -> False
  StarF fb | filterWord phall pattrest -> True
           | subsetFB fb (featureBundle phtop) && filterWord phrest pattall -> True
           | otherwise -> False
  QuestionF fb | filterWord phall pattrest -> True
               | subsetFB fb (featureBundle phtop) && filterWord phrest pattrest -> True
               | otherwise -> False
  

-- |Filter a word as a 'Token' based on a pattern
filterToken :: PhonemicInventory -> [Pattern] -> Token -> Bool
filterToken inv patt tok = case segment inv tok of
  Nothing -> False
  Just x -> filterWord x patt
