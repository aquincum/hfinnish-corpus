-- |This module provides some semi-regexp tools to look for
-- or manipulate certain phonological patterns.
module Hanalyze.Pattern (
  -- * Data type
  Pattern(..),

  -- * String conversions
  readPattern, writePattern,

  -- * Filtering
  filterWord, filterToken,

  -- * Matching
  matchWord,

  -- * Merging
  mergeDotPatterns,

  -- * Generating
  generatePattern, generateOverlappedPatterns
  ) where

import Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import Hanalyze.Token (Token, pack, unpack)
import Data.List (intersperse)
import Text.Parsec
import Data.Maybe (mapMaybe)

type PattParser st x = Parsec String st x

-- |A pattern for filtering, lookup or even generating strings based on an inventory.
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


{-| Reads in a pattern based on a phonemic inventory. It handles digraphs,
   and some regexp-like tools.

 * a phoneme is matched by simply typing out a phoneme (digraphs work!):

 @
  readPattern inv "abii" == Just ['P' a, 'P' b, 'P' ii]
 @

 * Optional presence of 1 phoneme is marked with a @?@

 @
  readPattern inv "?bii" == Just ['Question', P b, P ii]
 @

 * Any one phoneme is marked with a @.@

 @
  readPattern inv ".bii" == Just ['Dot', P b, P ii]
 @

 * Choice between more phonemes is marked with square brackets, like @[b,d,g]@

 @
  readPattern inv "[a,e]bii" == Just ['AnyP' [a,e], P b, P ii]
 @

 * Zero or more phonemes is marked with a @*@

 @
  readPattern inv "*bii" == Just ['Star', P b, P ii]
 @

 * Feature matching is marked as listing a feature bundle between curly brackets
   and a dot @.@, a question @?@ or a star @*@ as described above. The feature
   listing must be separated by commas and plus/minus/underspecification is marked
   with a @+@, @-@ and @0@. No space may come between the @+-0@ and the feature
   name, but spaces may be around the commas.

 @
  readPattern inv "{+vowel, -high}.bii" == Just ['DotF' ('FeatureBundle' [+vowel, -high]), P b, P ii]
  readPattern inv "{+vowel, -high}?bii" == Just ['QuestionF' ('FeatureBundle' [+vowel, -high]), P b, P ii]
  readPattern inv "{+vowel, -high}*bii" == Just ['StarF' ('FeatureBundle' [+vowel, -high]), P b, P ii]
 @

-}
readPattern :: PhonemicInventory -> Token -> Maybe [Pattern]
readPattern inv s = case parse (parsePatterns inv) "pattern reading" (T.unpack s) of
  Left e -> Nothing
  Right x -> Just x
                     
-- |Outputs a pattern in a human-readable form
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
      skipMany (char ' ')
      pm <- parsePM
      fn <- many1 (noneOf ",{} ")
      skipMany (char ' ')
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
  
-- |Go as far in a word as possible to match
matchWord :: [Phoneme] -> [Pattern] -> Maybe [Phoneme]
matchWord _ [] = Just []
matchWord [] patt = case head patt of
  Question -> matchWord [] (tail patt)
  Star -> matchWord [] (tail patt)
  QuestionF _ -> matchWord [] (tail patt)
  StarF _ -> matchWord [] (tail patt)
  _ -> Nothing
matchWord phall@(phtop:phrest) pattall@(pattop:pattrest) = case pattop of
  P phon | phonemeName phon == phonemeName phtop -> matchWord phrest pattrest >>= \x -> return (phtop:x)
         | otherwise -> Nothing
  AnyP phons | length (filter (\p -> phonemeName p==phonemeName phtop) phons) > 0 -> matchWord phrest pattrest >>= \x -> return (phtop:x)
             | otherwise -> Nothing
  DotF fb | subsetFB fb (featureBundle phtop) -> matchWord phrest pattrest >>= \x -> return (phtop:x)
          | otherwise -> Nothing
  Dot -> matchWord phrest pattrest >>= \x -> return (phtop:x)
  Question -> case matchWord phrest pattrest of
    Nothing -> matchWord phall pattrest
    Just succ -> return (phtop:succ)
  Star -> case matchWord phrest pattall of
    Nothing -> matchWord phall pattrest
    Just succ -> return (phtop:succ)
  StarF fb -> case matchWord phrest pattall of
    Just succ | subsetFB fb (featureBundle phtop) -> return (phtop:succ)
              | otherwise -> matchWord phall pattrest
    Nothing -> matchWord phall pattrest
  QuestionF fb -> case matchWord phall pattrest of
    Nothing -> case matchWord phrest pattrest of
      Just x | subsetFB fb (featureBundle phtop) -> return (phtop:x)
      otherwise -> Nothing
    Just succ -> return succ




-- |Filter a word as a 'Token' based on a pattern
filterToken :: PhonemicInventory -> [Pattern] -> Token -> Bool
filterToken inv patt tok = case segment inv tok of
  Nothing -> False
  Just x -> filterWord x patt



-- |Merge two patterns with Dots or DotF's only ~ get the intersection
-- of two strictly local grammars
mergeDotPatterns :: [Pattern] -- ^Pattern 1
                    -> [Pattern] -- ^Pattern 2
                    -> Maybe [Pattern]  -- ^The intersection, if the patterns conformed the Dottiness
mergeDotPatterns [] [] = Just []
mergeDotPatterns ps1 [] = Just ps1
mergeDotPatterns [] ps2 = Just ps2
mergeDotPatterns (p1:ps1) (p2:ps2) =
  -- Problem with 0's
  case (p1,p2) of
    (Dot, Dot) -> mergeDotPatterns ps1 ps2 >>= \x -> Just (Dot:x)
    (DotF f1, Dot) -> mergeDotPatterns ps1 ps2 >>= \x -> Just (DotF f1:x)
    (Dot, DotF f2) -> mergeDotPatterns ps1 ps2 >>= \x -> Just (DotF f2:x)
    (DotF f1, DotF f2) -> do
      x <- mergeDotPatterns ps1 ps2
      intersection <- intersectBundle f1 f2
      return $ DotF intersection:x
    (_,_) -> Nothing


-- |Generates all possible strings based on a dot pattern.
generatePattern :: PhonemicInventory -- ^The inventory
                   ->  [Pattern] -- ^The pattern
                   -> Maybe [[Phoneme]] -- ^The words!
generatePattern pi patt =
  case sequence $ generatedFBs patt of
    Nothing -> Nothing
    Just fbs -> Just $ generateFromFBs pi fbs

generatedFBs :: [Pattern] -> [Maybe FeatureBundle]
generatedFBs [] = []
generatedFBs (p:ps) = myFB p:(generatedFBs ps)

myFB p = case p of
  DotF fb -> Just fb
  Dot -> Just $ setBundle []
  _ -> Nothing


-- |Generates the overlapped pattern between two dot patterns.
overlappedPattern :: [Pattern]
                  -> [Pattern]
                  -> [[Pattern]]
overlappedPattern p1 p2 =
  let
    (shorter, longer) = if length p1 < length p2 then (p1,p2) else (p2,p1)
    pad patt len start = let pattlen = length patt
                             padend = len - start - pattlen
                         in
                          replicate start Dot ++ patt ++ replicate padend Dot
    npatterns = length longer - length shorter
    paddedshorts = map (pad shorter (length longer)) [0..npatterns]
    mergeds = mapMaybe (mergeDotPatterns longer) paddedshorts
  in
   mergeds


-- |Generates words based on the overlap of two dot patterns.
-- Careful with this, could be slow.
generateOverlappedPatterns :: PhonemicInventory
                          -> [Pattern]
                          -> [Pattern]
                          -> [[Phoneme]]
generateOverlappedPatterns pi p1 p2 =
  let
    mergeds = overlappedPattern p1 p2
    wordsGenerated = mapMaybe (generatePattern pi) mergeds
  in
   concat wordsGenerated

