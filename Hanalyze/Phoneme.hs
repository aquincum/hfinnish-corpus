module Hanalyze.Phoneme where

import Data.Monoid
import Data.Maybe
import Control.Monad
import qualified Hanalyze.Token as T
import Hanalyze.Token (Token)

-- |Plus or minus of a binary feature
data PlusMinus = Plus | Minus | Null deriving (Show,Eq)

-- |Merging features will involve merging [+-] values.
-- This will be done in a monoid style: if a feature is
-- not present in the stack, it will be set to [+-], but
-- a + and a - will kill each other to generalize
instance Monoid PlusMinus where
  mempty = Null
  mappend x y | x == y = x
  mappend Null x = x
  mappend x Null = x
  mappend Plus Minus = Null
  mappend Minus Plus = Null
  mappend x y = error $ "I don't understand " ++ show x ++ " + " ++ show y

-- |Complementer feature, turns 'Plus' to 'Minus', and 'Minus' to 'Plus', but
-- leaves 'Null' alone
minusPM :: PlusMinus -> PlusMinus
minusPM Plus = Minus
minusPM Minus = Plus
minusPM Null = Null


-- |A feature, whose value can be +, - or 0 (undefined)
data Feature = F { plusMinus :: PlusMinus, featureName :: String } deriving (Show,Eq)

-- 'minusPM' lifted to Feature
minus :: Feature -> Feature
minus (F pm fn) = F (minusPM pm) fn

-- |Null a feature
underspecified :: Feature -> Feature
underspecified (F pm fn) = F Null fn

-- |A feature bundle. Will be used as a monoid.
newtype FeatureBundle = Bundle {
  innerBundle :: [Feature]
} deriving (Show)

-- |If I reimplement 'FeatureBundle' as sets, I want to hide the type constructor
setBundle :: [Feature] -> FeatureBundle
setBundle l =
  let
    unique :: [Feature] -> [String] -> [Feature]
    unique [] _ = []
    unique list names =
      let
        topF = head list
        fn = featureName topF
        tailF = tail list
      in
       if fn `elem` names
       then unique tailF names
       else topF : unique tailF (fn:names)
  in     
   Bundle $ unique l []

-- |If I reimplement 'FeatureBundle' as sets, I want to hide the getter
getBundle :: FeatureBundle -> [Feature]
getBundle = innerBundle

-- |Merging bundles
instance Monoid FeatureBundle where
  mempty = Bundle []
  mappend = mergeBundle

-- |This is where the feature merging happens. The output has to be a 'FeatureBundle' as
-- two different features just get lumped together
mergeFeature :: Feature -> Feature -> FeatureBundle
mergeFeature f1 f2 = if featureName f1 == featureName f2 then
                       Bundle [F (plusMinus f1 `mappend` plusMinus f2) (featureName f1)]
                     else
                       Bundle [f1,f2]

-- |Alternative: merge two features, assuming they have an equal name. I'll just
-- return a Maybe value..
mergeFeature' :: Feature -> Feature -> Maybe Feature
mergeFeature' f1 f2 = if featureName f1 == featureName f2 then
                        Just $ F (plusMinus f1 `mappend` plusMinus f2) (featureName f1)
                      else
                        Nothing


-- |Find a 'Feature' by name in a 'FeatureBundle'
findInBundle :: String -> FeatureBundle -> Maybe Feature
findInBundle s fb = case getBundle fb of
  [] -> Nothing
  (x:xs) -> if featureName x == s
            then Just x
            else findInBundle s (Bundle xs) -- ugly
               
-- |Merges two bundles so that there is no duplication. It might fail if a
-- feature with name "" is used -- please don't do that!
mergeBundle :: FeatureBundle -> FeatureBundle -> FeatureBundle
mergeBundle b1 b2 = let f1 = getBundle b1
                        f2 = getBundle b2
                        allinf1 = foldr (\feat list ->
                                                      let foundfeat = findInBundle (featureName feat) b2 in
                                                      case foundfeat of
                                                        Just x -> fromJust (mergeFeature' feat x) : list -- I can use fromJust as I know it will be the same
                                                        Nothing -> feat : list
                                                    ) [] f1
                        notinf1 = filter (\feat -> isNothing $ findInBundle (featureName feat) b1) f2
                    in
                     Bundle $ allinf1 ++ notinf1


-- |The definition of a phoneme consists of the textual representation
-- and the feature bundle defining the phoneme.
data Phoneme = Phoneme {
  phonemeName :: String,
  featureBundle :: FeatureBundle
} deriving (Show)

type PhonemicInventory = [Phoneme] 

-- I'm just putting these here for now
fLabial = F Plus "labial"
fCoronal = F Plus "coronal"
fVelar = F Plus "velar"
fGlottal = F Plus "glottal"
fPalatal = F Plus "palatal"

fContinuant = F Plus "continuant"
fSonorant =  F Plus "sonorant"
fCons = F Plus "consonantal"
fVowel = minus fCons

fVoiced = F Plus "voiced"
fVoiceless = F Minus "voiced"

fHigh = F Plus "high"
fLow = F Plus "low"
fRounded = F Plus "rounded"

fFront = F Plus "front"
fLong = F Plus "long"


labial = Bundle [fLabial, underspecified fCoronal, underspecified fVelar, fCons]
coronal = Bundle [underspecified fLabial,  fCoronal, underspecified fVelar, fCons]
velar = Bundle [underspecified fLabial, underspecified fCoronal,  fVelar, fCons]
glottal = Bundle [fGlottal, fCons]
palatal = mconcat [Bundle [fPalatal], Bundle $ map underspecified [fLabial, fVelar, fGlottal, fCoronal]]

voiced = Bundle [fVoiced, fCons]
voiceless = Bundle [minus fVoiced, fCons]

stop = Bundle [minus fContinuant, minus fSonorant, fCons]
nasal = Bundle [minus fContinuant, fSonorant, fCons, fVoiced]
fricative = Bundle [fContinuant, minus fSonorant, fCons]
approximant = Bundle [fContinuant, fSonorant, fCons, fVoiced]

lateral = Bundle [F Plus "lateral"]
trill = Bundle [F Plus "trill"]

short = Bundle [minus fLong]
long = Bundle [fLong]

-- vowels
high = Bundle [fHigh, minus fLow, fVowel]
mid = Bundle [minus fHigh, minus fLow, fVowel]
low = Bundle [minus fHigh, fLow, fVowel]

rounded = Bundle [fRounded]
unrounded = Bundle [minus fRounded]

front = Bundle [fFront]
back = Bundle [minus fFront]

-- |Testing with a mock Finnish inventory
testInv :: PhonemicInventory
testInv = [
  Phoneme "p" (mconcat [voiceless, labial, stop]),
  Phoneme "b" (mconcat [voiced, labial, stop]),
  Phoneme "t" (mconcat [voiceless, coronal, stop]),
  Phoneme "d" (mconcat [voiced, coronal, stop]),
  Phoneme "k" (mconcat [voiceless, velar, stop]),
  Phoneme "g" (mconcat [voiced, velar, stop]),
  Phoneme "m" (mconcat [nasal, labial]),
  Phoneme "n" (mconcat [nasal, coronal]),
  Phoneme "ng" (mconcat [nasal, velar]),
  Phoneme "f" (mconcat [fricative, voiceless, labial]),
  Phoneme "v" (mconcat [fricative, voiced, labial]),
  Phoneme "s" (mconcat [fricative, voiceless, coronal]),
  Phoneme "z" (mconcat [fricative, voiced, coronal]),
  Phoneme "š" (mconcat [fricative, voiceless, coronal]),
  Phoneme "ž" (mconcat [fricative, voiced, coronal]),
  Phoneme "h" (mconcat [fricative, glottal, voiceless]),
  Phoneme "l" (mconcat [approximant, lateral, coronal]),
  Phoneme "r" (mconcat [approximant, trill, coronal]),
  Phoneme "j" (mconcat [approximant, palatal]),
  Phoneme "a" (mconcat [back, low, unrounded]), 
  Phoneme "o" (mconcat [back, mid, rounded]),
  Phoneme "u" (mconcat [back, high, rounded]),
  Phoneme "ä" (mconcat [front, low, unrounded]),
  Phoneme "e" (mconcat [front, mid, unrounded]),
  Phoneme "i" (mconcat [front, high, unrounded]),
  Phoneme "ö" (mconcat [front, mid, rounded]),
  Phoneme "y" (mconcat [front, high, rounded])
 ]

finnishInventory :: PhonemicInventory
finnishInventory = mapWith testInv short ++ mapWith doubledInv long ++ [
  Phoneme "ie" (mconcat [front, high, unrounded, long]),
  Phoneme "ei" (mconcat [front, mid, unrounded, long])
  ]
  where
    mapWith inv feat = map (\phon -> Phoneme (phonemeName phon) (mconcat [featureBundle phon, feat]) ) inv
    doubledInv = map (\phon -> let n = phonemeName phon in
                       Phoneme (n ++ if n == "ng" then "" else n) (featureBundle phon)) testInv

findPhoneme :: PhonemicInventory -> String -> Maybe Phoneme
findPhoneme [] _ = Nothing
findPhoneme (x:xs) s = if phonemeName x == s
                       then Just x
                       else findPhoneme xs s

-- |Segment a token, so far only with _digraphs_
segment :: PhonemicInventory -> Token -> Maybe [Phoneme]
segment inv t = innersegment (T.unpack t)
  where -- innersegment with String representation for now
    lookupDigraph :: (Char, Char) -> Maybe Phoneme
    lookupDigraph (l, r) = case findPhoneme inv [l,r] of
      Nothing -> Nothing
      Just ph -> Just ph
    innersegment :: String -> Maybe [Phoneme]
    innersegment [] = Just []
    innersegment (x:xs) =
      let (foundphoneme, rest) = 
            if xs /= [] then -- check for digraphs
              case lookupDigraph (x,head xs) of
                Nothing -> (findPhoneme inv [x], xs)
                Just dig -> (Just dig, tail xs)
            else
              (findPhoneme inv [x],[])
      in
       case foundphoneme of
         Nothing -> Nothing
         Just ph -> liftM2 (:) (Just ph)  (innersegment rest)


data Pattern = P Phoneme | Dot | Star | Question

filterWord :: [Phoneme] -> [Pattern] -> Bool
filterWord [] [] = True
filterWord ph [] = False
filterWord [] patt = False
filterWord phall@(phtop:phrest) patall@(pattop:pattrest) = case pattop of
  P phon | phonemeName phon == phonemeName phtop -> True
         | otherwise -> False
  Dot -> filterWord phrest pattrest
  Question | filterWord phrest pattrest -> True
           | filterWord phall pattrest -> True
           | otherwise -> False
  Star | filterWord phall pattrest -> True
       | filterWord phrest patall -> True
       | otherwise -> False


