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
import Data.List (intersperse)
import Text.Parsec


type PattParser st x = Parsec String st x

-- |A pattern for filtering
data Pattern = P Phoneme -- ^Matches a given phoneme based on its label
             | AnyP [Phoneme] -- ^Matches any of the listed phonemes
             | Dot -- ^Matches one and one phoneme only
             | Star -- ^Matches 0 or more phonemes
             | Question -- ^Matches 0 or 1 phoneme
             | DotF FeatureBundle -- ^Matches a feature bundle
             | StarF FeatureBundle -- ^Matches 0 or more phonemes that match the feature bundle
             | QuestionF FeatureBundle -- ^Matches 0 or 1 phonemes that match the feature bundle
               deriving (Eq)

instance Show Pattern where
  show patt = writePattern [patt]


-- |Read in a pattern based on a phonemic inventory. Now with quality parsing!
readPattern :: PhonemicInventory -> Token -> Maybe [Pattern]
readPattern inv s = case parse (parsePatterns inv) "pattern reading" (T.unpack s) of
  Left e -> Nothing
  Right x -> Just x
                     
-- |Output a pattern in a human-readable form
writePattern :: [Pattern] -> String
writePattern = concatMap write'
  where
    write' x = case x of
      Star -> "*"
      Question -> "?"
      Dot -> "."
      P y -> phonemeName y
      AnyP plist -> "[" ++ foldl (++) "" (map phonemeName plist) ++ "]."
      DotF fb -> "{" ++ foldl (++) "" (intersperse "," (map show (getBundle fb))) ++ "}."
      StarF fb -> "{" ++ foldl (++) "" (intersperse "," (map show (getBundle fb))) ++ "}*"
      QuestionF fb -> "{" ++ foldl (++) "" (intersperse "," (map show (getBundle fb))) ++ "}?"

parsePatterns :: PhonemicInventory -> PattParser st [Pattern]
parsePatterns pi = many1 $ parsePattern pi

parsePattern :: PhonemicInventory -> PattParser st Pattern
parsePattern pi = try digraph <|> star <|> question <|> dot <|>
                  p <|> anyp <|>
                  (do
                      fl <- featlist
                      (char '.' >> return (DotF fl)) <|>
                        (char '?' >> return (QuestionF fl)) <|>
                        (char '*' >> return (StarF fl))
                    ) <|>
                  ( digit >> unexpected "Number in pattern" ) <|>
                  ( space >> unexpected "Whitespace in pattern" ) <|>
                  ( noneOf "" >> fail "Unexpected character")
  where
    anyp = (AnyP . foldl (\acc (P x) -> (acc++[x])) [])
           `fmap`
           between (char '[') (char ']') (sepBy (try digraph <|> p) (char ','))
    featlist = setBundle
               `fmap`
               between (char '{') (char '}') featureSpecifications
    star = char '*' >> return Star
    question = char '?' >> return Question
    dot = char '.' >> return Dot
    p = letter >>= \l -> phoneme [l]
    digraph = letter >>= \a -> letter >>= \b -> phoneme [a,b]
    phoneme p = case findPhoneme pi p of
      Just x -> return $ P x
      Nothing -> unexpected $ "No such phoneme " ++ p
    parsePM = (char '+' >> return Plus) <|>
              (char '-' >> return Minus) <|>
              (char '0' >> return Null) <|>
              return Plus -- ?
    featureSpecifications :: PattParser st [Feature]
    featureSpecifications = sepBy featureSpecification (char ',')
    featureSpecification :: PattParser st Feature
    featureSpecification = do
      pm <- parsePM
      fn <- many1 (noneOf ",{}")
      return $ Feature pm fn

--    getPhoneme p = 


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
  AnyP phons | length (filter (\p -> phonemeName p==phonemeName phtop) phons) > 0 -> filterWord phrest pattrest
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
