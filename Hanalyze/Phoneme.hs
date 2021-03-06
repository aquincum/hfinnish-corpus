{-# LANGUAGE OverloadedStrings #-}

module Hanalyze.Phoneme
       (
         -- * Data types
         PlusMinus(..), Feature(..), FeatureBundle,
         Phoneme(..), PhonemicInventory,

         -- * Basic functions
         -- ** With 'PlusMinus'
         minusPM,

         -- ** With 'Feature'
         minus, underspecified,

         -- ** With 'FeatureBundle'
         emptyBundle, setBundle, getBundle,

         -- * Merging features and bundles
         mergeFeature, mergeFeature', mergeBundle, intersectBundle, subsetFB,

         -- * Lookup functions
         findInBundle, findPhoneme, isPhoneme,

         -- * Segmenting
         segment, segmentWords, spellout,

         -- * Generating
         generateFromFBs,
                       
         -- * Feature system
         -- ** basic features
         -- *** consonants
         fLabial, fCoronal, fVelar, fGlottal, fPalatal, fContinuant,
         fSonorant, fCons, fVoiced, fVoiceless, fWordBoundary,

         -- *** vowels
         fVowel, fHigh, fLow,
         fRounded, fFront, fLong,

         -- ** as feature bundles
         -- *** consonants
         labial, coronal, velar, glottal, palatal, voiced, voiceless,
         stop, nasal, fricative, approximant, lateral, trill, short,
         long, consonant, singleton, geminate,

         -- *** vowels
         high, mid, low, rounded, unrounded, front, back, vowel,

         -- *** boundaries
         word_boundary,

         -- * Inventories
         finnishInventory, finnishInventoryFullDiphthongs,
         finnishInventoryNoDiphthongs, finnishInventoryBFN,
         finnishInventoryBFNEIIE, finnishInventoryBFNFullDiphthongs,
         finnishInventoryBFNOnly, finnishInventoryBFNOnlyEIIE,
         finnishInventoryBFNOnlyFullDiphthongs,
         finnishInventoryBFNreplaced,
         addFBNExclToInventory,
         
         -- ** Operations on inventories
         selectRelevantBundles, listFeatures,
         listBundles, pickByFeature, filterInventory,
         filterInventoryByBundle,

         -- ** Added word boundary
         finnishInventoryWithEdges, augmentWord,
         addEdgeToInventory
         

         ) where

import           Control.Monad
import           Control.Monad.Writer
import           Data.Function (on)
import           Data.List (delete,nub,sortBy,minimumBy,groupBy)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as Txt
import           Hanalyze.Token (Token)
import qualified Hanalyze.Token as T

-- |Plus or minus of a binary feature
data PlusMinus = Plus | Minus | Null deriving (Eq)

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

instance Show PlusMinus where
  show Plus = "+"
  show Minus = "-"
  show Null = "0"

-- |Complementer feature, turns 'Plus' to 'Minus', and 'Minus' to 'Plus', but
-- leaves 'Null' alone
minusPM :: PlusMinus -> PlusMinus
minusPM Plus = Minus
minusPM Minus = Plus
minusPM Null = Null



-- |A feature, whose value can be +, - or 0 (undefined)
data Feature = Feature { plusMinus :: PlusMinus, featureName :: String } deriving (Eq)


instance Show Feature where
  show f = show (plusMinus f) ++ featureName f

-- |'minusPM' lifted to Feature
minus :: Feature -> Feature
minus (Feature pm fn) = Feature (minusPM pm) fn




-- |Null a feature
underspecified :: Feature -> Feature
underspecified (Feature pm fn) = Feature Null fn

-- |A feature bundle. Will be used as a monoid.
newtype FeatureBundle = Bundle {
  innerBundle :: [Feature]
} deriving (Eq)


instance Show FeatureBundle where
  show = show . innerBundle

emptyBundle :: FeatureBundle
emptyBundle = Bundle []

-- |If I reimplement 'FeatureBundle' as sets, I want to hide the type constructor
setBundle :: [Feature] -> FeatureBundle
setBundle l =
  let
    unique :: [Feature] -> [String] -> [Feature]
    unique [] _ = []
    unique list names =
      let
        topFeature = head list
        fn = featureName topFeature
        tailFeature = tail list
      in
       if fn `elem` names
       then unique tailFeature names
       else topFeature : unique tailFeature (fn:names)
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
                       Bundle [Feature (plusMinus f1 `mappend` plusMinus f2) (featureName f1)]
                     else
                       Bundle [f1,f2]

-- |Alternative: merge two features, assuming they have an equal name. I'll just
-- return a Maybe value..
mergeFeature' :: Feature -> Feature -> Maybe Feature
mergeFeature' f1 f2 = if featureName f1 == featureName f2 then
                        Just $ Feature (plusMinus f1 `mappend` plusMinus f2) (featureName f1)
                      else
                        Nothing



-- |Find a 'Feature' by name in a 'FeatureBundle'
findInBundle :: String -> FeatureBundle -> Maybe Feature
findInBundle s fb = case getBundle fb of
  [] -> Nothing
  (x:xs) -> if featureName x == s
            then Just x
            else findInBundle s (Bundle xs) -- ugly
               
-- |Abstract internal merging function, will be specialized based on the
-- different uniting functions.
uniteBundle :: (Feature -> Feature -> Maybe Feature) -- ^A function merging two features
               -> FeatureBundle -- ^FB1
               -> FeatureBundle -- ^FB2
               -> Maybe FeatureBundle -- ^result
uniteBundle unite b1 b2 = do
  let f1 = getBundle b1
      f2 = getBundle b2
{-
      
      allinf1 = foldr (\feat list ->
                        let foundfeat = findInBundle (featureName feat) b2 in
                        case foundfeat of
                          Just x -> case unite feat x of
                            Just united -> Just united : list
                            Nothing -> Nothing
                          Nothing -> Just feat : list
                      ) [] f1

-}
      notinf1 = filter (\feat -> isNothing $ findInBundle (featureName feat) b1) f2
  allinf1 <- foldM (\list feat -> 
                        let foundfeat = findInBundle (featureName feat) b2
                        in
                         case foundfeat of
                           Just x -> do
                             united <- unite feat x
                             return $ united : list
                           Nothing -> Just $ feat : list
                      ) [] f1

  return $ Bundle $ allinf1 ++ notinf1


-- |Merges two bundles so that there is no duplication. It might fail if a
-- feature with an empty string name  is used -- please don't do that!
mergeBundle :: FeatureBundle -> FeatureBundle -> FeatureBundle
mergeBundle b1 b2 = fromJust (uniteBundle mergeFeature' b1 b2)


-- | Merges two bundles so that a Plus and a Minus extinguish each other!
-- While 'mergeBundle' generalizes, this one intersects.
intersectBundle :: FeatureBundle -> FeatureBundle -> Maybe FeatureBundle
intersectBundle = uniteBundle intersectFeature
  where
    intersectFeature f1 f2 =
      let fname = featureName f1 in
      if fname /= featureName f2
      then Nothing
      else case (plusMinus f1, plusMinus f2) of
        (Plus, Plus) -> Just $ Feature Plus fname
        (Minus, Minus) -> Just $ Feature Minus fname
        (Plus, Null) -> Just $ Feature Plus fname
        (Minus, Null) -> Just $ Feature Minus fname
        (Null, Plus) -> Just $ Feature Plus fname
        (Null, Minus) -> Just $ Feature Minus fname
        (Null, Null) -> Just $ Feature Null fname
        (_,_) -> Nothing

-- |Whether the first 'FeatureBundle' is the subset of the second
-- one
subsetFB :: FeatureBundle -> FeatureBundle -> Bool
subsetFB f1 f2 | null (getBundle f1) = True
subsetFB f1 f2 | null (getBundle f2) = False
subsetFB f1 f2 = let (firstf:tailf) = getBundle f1
                     fn = featureName firstf
                     pm = plusMinus firstf
                 in
                  if pm == Null
                  then subsetFB (Bundle tailf) f2
                  else
                    case findInBundle fn f2 of
                      Nothing -> False
                      Just foundf ->
                        if plusMinus firstf == plusMinus foundf
                        then subsetFB (Bundle tailf) f2
                        else False


-- |The definition of a phoneme consists of the textual representation
-- and the feature bundle defining the phoneme.
data Phoneme = Phoneme {
  phonemeName :: String,
  featureBundle :: FeatureBundle
} deriving (Show, Eq)

type PhonemicInventory = [Phoneme] 

-- I'm just putting these here for now
fLabial = Feature Plus "labial"
fCoronal = Feature Plus "coronal"
fVelar = Feature Plus "velar"
fGlottal = Feature Plus "glottal"
fPalatal = Feature Plus "palatal"

--fAnterior = Feature Plus "anterior"

fContinuant = Feature Plus "continuant"
fSonorant =  Feature Plus "sonorant"
-- fCons = Feature Plus "consonantal"
fCons = Feature Minus "syllabic"
fVowel = minus fCons

fVoiced = Feature Plus "voiced"
fVoiceless = Feature Minus "voiced"

fHigh = Feature Plus "high"
fLow = Feature Plus "low"
fRounded = Feature Plus "rounded"

fFront = Feature Plus "front"
fLong = Feature Plus "long"
fGeminate = Feature Plus "geminate"

fWordBoundary = Feature Plus "word_boundary"
fRaising = Feature Plus "raising" -- diphthongs, ei, ai, \"ai, \"oi, oi, ui (!) (!), yi (!), ey, \"oy, \"ay, eu, au, ou, iy (!), iu (!)
fLowering = minus fRaising -- diphthongs, ie, y\"o, uo
fRounding = Feature Plus "rounding" -- ey, \"oy, \"ay, eu, au, ou, iy, iu
fUnrounding = minus fRounding -- ...i diphthongs
fFronting = Feature Plus "fronting" -- i/y diphthonhs
fBacking = minus fFronting -- u diphthongs
fDiphthong = Feature Plus "diphthong"



labial = Bundle [fLabial, underspecified fCoronal, underspecified fVelar, fCons]
coronal = Bundle [underspecified fLabial,  fCoronal, underspecified fVelar, fCons]
velar = Bundle [underspecified fLabial, underspecified fCoronal,  fVelar, fCons]
glottal = Bundle [fGlottal, fCons]
palatal = mconcat [Bundle [fPalatal], Bundle $ map underspecified [fLabial, fVelar, fGlottal, fCoronal]]
--anterior = Bundle [fAnterior]
--posterior = Bundle [minus fAnterior]


voiced = Bundle [fVoiced, fCons]
voiceless = Bundle [minus fVoiced, fCons]

stop = Bundle [minus fContinuant, minus fSonorant, fCons]
nasal = Bundle [minus fContinuant, fSonorant, fCons, fVoiced]
fricative = Bundle [fContinuant, minus fSonorant, fCons]
approximant = Bundle [fContinuant, fSonorant, fCons, fVoiced]

lateral = Bundle [Feature Plus "lateral"]
trill = Bundle [Feature Plus "trill"]

consonant = Bundle [fCons]

singleton = Bundle [minus fGeminate]
geminate = Bundle [fGeminate]

-- vowels
short = Bundle [minus fLong]
long = Bundle [fLong]
vowel = Bundle [fVowel]
high = Bundle [fHigh, minus fLow, fVowel]
mid = Bundle [minus fHigh, minus fLow, fVowel]
low = Bundle [minus fHigh, fLow, fVowel]

rounded = Bundle [fRounded, fVowel]
unrounded = Bundle [minus fRounded, fVowel]

front = Bundle [fFront, fVowel]
back = Bundle [minus fFront, fVowel]

diphthong = Bundle [fDiphthong]
raising = Bundle [fDiphthong, fRaising]
lowering = Bundle [fDiphthong, fLowering]
rounding = Bundle [fDiphthong, fRounding]
unrounding = Bundle [fDiphthong, fUnrounding]
fronting = Bundle [fDiphthong, fFronting]
backing = Bundle [fDiphthong, fBacking]


bfnLowering = Bundle [fLowering] -- !no fDiphthong
bfnUDiphthong = Bundle [Feature Plus "udiphthong"]
bfnIDiphthong = Bundle [Feature Plus "idiphthong"]
bfnYDiphthong = Bundle [Feature Plus "ydiphthong"]



-- other

word_boundary = Bundle [fWordBoundary]
phoneme = Bundle [minus fWordBoundary]

-- |Testing with a mock Finnish inventory. These are the consonants
testInvConsonants :: PhonemicInventory
testInvConsonants = [
  Phoneme "p" (mconcat [voiceless, labial, stop]),
  Phoneme "b" (mconcat [voiced, labial, stop]),
  Phoneme "t" (mconcat [voiceless, coronal, stop]),
  Phoneme "d" (mconcat [voiced, coronal, stop]),
  Phoneme "k" (mconcat [voiceless, velar, stop]),
  Phoneme "g" (mconcat [voiced, velar, stop]),
  Phoneme "m" (mconcat [nasal, labial]),
  Phoneme "n" (mconcat [nasal, coronal]),
  Phoneme "f" (mconcat [fricative, voiceless, labial]),
  Phoneme "v" (mconcat [fricative, voiced, labial]),
  Phoneme "s" (mconcat [fricative, voiceless, coronal]),
  Phoneme "z" (mconcat [fricative, voiced, coronal]),
  --  Phoneme "š" (mconcat [fricative, voiceless, coronal, posterior]),
  -- Phoneme "ž" (mconcat [fricative, voiced, coronal, posterior]),
  Phoneme "h" (mconcat [fricative, glottal, voiceless]),
  Phoneme "l" (mconcat [approximant, lateral, coronal]),
  Phoneme "r" (mconcat [approximant, trill, coronal]),
  Phoneme "j" (mconcat [approximant, palatal])
  ]
                    
-- |Testing with a mock Finnish inventory. These are the vowels
testInvVowels = [
               Phoneme "a" (mconcat [back, low, unrounded]), 
               Phoneme "o" (mconcat [back, mid, rounded]),
               Phoneme "u" (mconcat [back, high, rounded]),
               Phoneme "ä" (mconcat [front, low, unrounded]),
               Phoneme "e" (mconcat [front, mid, unrounded]),
               Phoneme "i" (mconcat [front, high, unrounded]),
               Phoneme "ö" (mconcat [front, mid, rounded]),
               Phoneme "y" (mconcat [front, high, rounded])
               ]

bfnfBack = Feature Plus "Back"
bfnfNeutral = Feature Plus "Neutral"
bfnfFront = Feature Plus "Front"

bfnBack = Bundle $ [bfnfBack]
bfnNeutral = Bundle $ [bfnfNeutral]
bfnFront = Bundle $ [bfnfFront]

-- |With BFN features instead of +-front and +-rounded
testInvVowelsBFN = [
               Phoneme "a" (mconcat [bfnBack, low]), 
               Phoneme "o" (mconcat [bfnBack, mid]),
               Phoneme "u" (mconcat [bfnBack, high]),
               Phoneme "ä" (mconcat [bfnFront, low]),
               Phoneme "e" (mconcat [bfnNeutral, mid]),
               Phoneme "i" (mconcat [bfnNeutral, high]),
               Phoneme "ö" (mconcat [bfnFront, mid]),
               Phoneme "y" (mconcat [bfnFront, high])
               ]

-- |With BFN features ONLY
testInvVowelsBFNOnly = [
               Phoneme "a" (mconcat [vowel, bfnBack]), 
               Phoneme "o" (mconcat [vowel, bfnBack]),
               Phoneme "u" (mconcat [vowel, bfnBack]),
               Phoneme "ä" (mconcat [vowel, bfnFront]),
               Phoneme "e" (mconcat [vowel, bfnNeutral]),
               Phoneme "i" (mconcat [vowel, bfnNeutral]),
               Phoneme "ö" (mconcat [vowel, bfnFront]),
               Phoneme "y" (mconcat [vowel, bfnFront])
               ]


testInv = testInvConsonants ++ testInvVowels

-- |Maps a feature on a whole inventory
mapWithFeat :: [Phoneme] -> FeatureBundle -> [Phoneme]
mapWithFeat inv feat = map (\phon -> Phoneme (phonemeName phon) (mconcat [featureBundle phon, feat]) ) inv

-- |The basic Finnish inventory with no diphthongs
finnishInventoryBase :: PhonemicInventory
finnishInventoryBase = mapWithFeat testInvVowels short ++
                   mapWithFeat testInvConsonants singleton ++
                   mapWithFeat (double testInvVowels) long ++
                   mapWithFeat (double testInvConsonants) geminate ++
                   [
  Phoneme "ng" (mconcat [nasal, velar, geminate])
  ]

finnishInventoryBFN :: PhonemicInventory
finnishInventoryBFN = mapWithFeat testInvVowelsBFN short ++
                      mapWithFeat (double testInvVowelsBFN) long ++
                      mapWithFeat testInvConsonants singleton ++
                      mapWithFeat (double testInvConsonants) geminate ++
                      [
                        Phoneme "ng" (mconcat [nasal, velar, geminate])
                      ]

finnishInventoryBFNOnly :: PhonemicInventory
finnishInventoryBFNOnly = testInvVowelsBFNOnly ++
                          double testInvVowelsBFNOnly ++
                          mapWithFeat testInvConsonants singleton ++
                          mapWithFeat (double testInvConsonants) geminate ++
                          [
                            Phoneme "ng" (mconcat [nasal, velar, geminate])
                          ]

finnishInventoryBFNreplaced :: PhonemicInventory
finnishInventoryBFNreplaced = mapWithFeat testInvConsonants singleton ++
                              mapWithFeat (double testInvConsonants) geminate ++
                              [
                                Phoneme "B" (mconcat [vowel, bfnBack]),
                                Phoneme "F" (mconcat [vowel, bfnFront]),
                                Phoneme "N" (mconcat [vowel, bfnNeutral])
                              ]

double :: [Phoneme] -> [Phoneme]
double = map (\phon -> let n = phonemeName phon in
               Phoneme (n ++ n) (featureBundle phon))


-- |Just like 'finnishInventoryBase', for export.
finnishInventoryNoDiphthongs :: PhonemicInventory
finnishInventoryNoDiphthongs = finnishInventoryBase


-- |Produces more or less the relevant Finnish phonemic inventory.
finnishInventory :: PhonemicInventory
finnishInventory = finnishInventoryBase ++
                   [
  Phoneme "ie" (mconcat [front, high, unrounded, long, diphthong]),
  Phoneme "ei" (mconcat [front, mid, unrounded, long, diphthong])
  ]


finnishInventoryBFNEIIE :: PhonemicInventory
finnishInventoryBFNEIIE = finnishInventoryBFN ++
                   [
  Phoneme "ie" (mconcat [bfnNeutral, high, long, diphthong]),
  Phoneme "ei" (mconcat [bfnNeutral, mid, long, diphthong])
  ]

finnishInventoryBFNOnlyEIIE :: PhonemicInventory
finnishInventoryBFNOnlyEIIE = finnishInventoryBFNOnly ++
                   [
  Phoneme "ie" (mconcat [vowel, bfnNeutral]),
  Phoneme "ei" (mconcat [vowel, bfnNeutral])
  ]

-- |The Finnish inventory with "ae" and "oe" instead of "ä" and "ö" respectively 
finnishInventoryAe :: PhonemicInventory
finnishInventoryAe = map replace finnishInventory
  where
    replace ph = ph {phonemeName = foldl replaceChar "" (phonemeName ph)}
    replaceChar sofar ch = case ch of
      'ä' -> sofar ++ "ae"
      'ö' -> sofar ++ "oe"
      _ -> sofar ++ [ch]


addEdgeToInventory :: PhonemicInventory -> String -> PhonemicInventory
addEdgeToInventory inv symbol= mapWithFeat inv phoneme  ++ [
    Phoneme symbol word_boundary
    ]

      
finnishInventoryWithEdges :: PhonemicInventory
finnishInventoryWithEdges = addEdgeToInventory finnishInventory "#"

finnishInventoryFullDiphthongs :: PhonemicInventory -- Iso Suomen Kielioppi
finnishInventoryFullDiphthongs = finnishInventoryBase ++
                                 [
                                   Phoneme "ai" (mconcat [back, low, unrounded, raising, fronting, unrounding]),
                                   Phoneme "oi" (mconcat [back, mid, rounded, raising, fronting, unrounding]),
                                   Phoneme "ui" (mconcat [back, high, rounded, raising, fronting, unrounding]),
                                   Phoneme "äi" (mconcat [front, low, unrounded, raising, fronting, unrounding]),
                                   Phoneme "ei" (mconcat [front, mid, unrounded, raising, fronting, unrounding]),
                                   Phoneme "öi" (mconcat [front, mid, rounded, raising, fronting, unrounding]),
                                   Phoneme "yi" (mconcat [front, high, rounded, raising, fronting, unrounding]),
                                   Phoneme "au" (mconcat [back, low, unrounded, raising, backing, rounding]),
                                   Phoneme "ou" (mconcat [back, mid, rounded, raising, backing, rounding]),
                                   Phoneme "iu" (mconcat [front, high, unrounded, raising, backing, rounding]),
                                   Phoneme "eu" (mconcat [front, mid, unrounded, raising, backing, rounding]),
                                   Phoneme "äy" (mconcat [front, low, unrounded, raising, fronting, rounding]),
                                   Phoneme "öy" (mconcat [front, mid, rounded, raising, fronting, rounding]),
                                   Phoneme "ey" (mconcat [front, mid, unrounded, raising, fronting, rounding]),
                                   Phoneme "iy" (mconcat [front, high, unrounded, raising, fronting, rounding]),
                                   Phoneme "yö" (mconcat [front, high, rounded, lowering]),
                                   Phoneme "ie" (mconcat [front, high, unrounded, lowering]),
                                   Phoneme "uo" (mconcat [back, high, rounded, lowering])
                                   -- ea, eä, oa are _not_ diphthongs according to ISK. Okay.
                                 ]


finnishInventoryBFNFullDiphthongs :: PhonemicInventory -- Iso Suomen Kielioppi
finnishInventoryBFNFullDiphthongs = finnishInventoryBFN ++
                                 [
                                   Phoneme "ai" (mconcat [bfnBack, low, bfnIDiphthong]),
                                   Phoneme "oi" (mconcat [bfnBack, mid, bfnIDiphthong]),
                                   Phoneme "ui" (mconcat [bfnBack, high, bfnIDiphthong]),
                                   Phoneme "äi" (mconcat [bfnFront, low, bfnIDiphthong]),
                                   Phoneme "ei" (mconcat [bfnNeutral, mid, bfnIDiphthong]),
                                   Phoneme "öi" (mconcat [bfnFront, mid, bfnIDiphthong]),
                                   Phoneme "yi" (mconcat [bfnFront, high, bfnIDiphthong]),
                                   Phoneme "au" (mconcat [bfnBack, low, bfnUDiphthong]),
                                   Phoneme "ou" (mconcat [bfnBack, mid, bfnUDiphthong]),
                                   Phoneme "iu" (mconcat [bfnNeutral, high, bfnUDiphthong]),
                                   Phoneme "eu" (mconcat [bfnNeutral, mid, bfnUDiphthong]),
                                   Phoneme "äy" (mconcat [bfnFront, low, bfnYDiphthong]),
                                   Phoneme "öy" (mconcat [bfnFront, mid, bfnYDiphthong]),
                                   Phoneme "ey" (mconcat [bfnNeutral, mid, bfnYDiphthong]),
                                   Phoneme "iy" (mconcat [bfnNeutral, high, bfnYDiphthong]),
                                   Phoneme "yö" (mconcat [bfnFront, high, lowering]),
                                   Phoneme "ie" (mconcat [bfnNeutral, high, lowering]),
                                   Phoneme "uo" (mconcat [bfnBack, high, lowering])
                                   -- ea, eä, oa are _not_ diphthongs according to ISK. Okay.
                                 ]

finnishInventoryBFNOnlyFullDiphthongs :: PhonemicInventory -- Iso Suomen Kielioppi
finnishInventoryBFNOnlyFullDiphthongs = finnishInventoryBFNOnly ++
                                 [
                                   Phoneme "ai" (mconcat [vowel, bfnBack]),
                                   Phoneme "oi" (mconcat [vowel, bfnBack]),
                                   Phoneme "ui" (mconcat [vowel, bfnBack]),
                                   Phoneme "äi" (mconcat [vowel, bfnFront]),
                                   Phoneme "ei" (mconcat [vowel, bfnNeutral]),
                                   Phoneme "öi" (mconcat [vowel, bfnFront]),
                                   Phoneme "yi" (mconcat [vowel, bfnFront]),
                                   Phoneme "au" (mconcat [vowel, bfnBack]),
                                   Phoneme "ou" (mconcat [vowel, bfnBack]),
                                   Phoneme "iu" (mconcat [vowel, bfnNeutral]),
                                   Phoneme "eu" (mconcat [vowel, bfnNeutral]),
                                   Phoneme "äy" (mconcat [vowel, bfnFront]),
                                   Phoneme "öy" (mconcat [vowel, bfnFront]),
                                   Phoneme "ey" (mconcat [vowel, bfnNeutral]),
                                   Phoneme "iy" (mconcat [vowel, bfnNeutral]),
                                   Phoneme "yö" (mconcat [vowel, bfnFront]),
                                   Phoneme "ie" (mconcat [vowel, bfnNeutral]),
                                   Phoneme "uo" (mconcat [vowel, bfnBack])
                                   -- ea, eä, oa are _not_ diphthongs according to ISK. Okay.
                                 ]

addFBNExclToInventory :: PhonemicInventory -> PhonemicInventory
addFBNExclToInventory inv = if "Back" `elem` (listFeatures inv)
                            then inv ++ [ -- BFN system
                              Phoneme "B" (mconcat [vowel, bfnBack]),
                              Phoneme "F" (mconcat [vowel, bfnFront]),
                              Phoneme "N" (mconcat [vowel, bfnNeutral]),
                              Phoneme "!" (setBundle [Feature Plus "error"])
                            ]
                            else inv ++ [ -- plain system, how to do N!?
                              Phoneme "B" (mconcat [vowel, back]), -- a
                              Phoneme "F" (mconcat [vowel, front, low]),
                              Phoneme "N" (mconcat [vowel, front, high]), -- ??
                              Phoneme "!" (setBundle [Feature Plus "error"])
                              ]



-- |Filters an Inventory according to a general predicate
filterInventory :: PhonemicInventory -> (Phoneme -> Bool) -> PhonemicInventory
filterInventory pi pred = filter pred pi

-- |Filters an Inventory according to a feature bundle that phonemes
-- have to match on to get in the resulting inventory
filterInventoryByBundle :: PhonemicInventory -> FeatureBundle -> PhonemicInventory
filterInventoryByBundle pi bund = filterInventory pi (subsetFB bund . featureBundle)


-- |Locates a phoneme in a phonemic inventory
findPhoneme :: PhonemicInventory -> String -> Maybe Phoneme
findPhoneme [] _ = Nothing
findPhoneme (x:xs) s = if phonemeName x == s
                       then Just x
                       else findPhoneme xs s

-- |Determines whether a phoneme has the feature values specified in the 'FeatureBundle'
isPhoneme :: Phoneme -> FeatureBundle -> Bool
isPhoneme ph fb = subsetFB fb (featureBundle ph)

                            
-- |Picks out a set of phonemes based on features
pickByFeature :: PhonemicInventory -> FeatureBundle -> [Phoneme]
pickByFeature pi fb = filter (\p -> fb `subsetFB` featureBundle p) pi

-- |Lists all the feature names in an inventory
listFeatures :: PhonemicInventory -> [String]
-- pi >>= getBundle . featureBundle === join $ map (getBundle . featureBundle) pi
-- in the [] monad
-- so we start unique the featurenames from the flattened feature bundles from the
-- phonemic inventory
listFeatures  = nub . (map featureName) . (>>= getBundle . featureBundle)

-- |Lists all bundles with a maximum length of @n@ that is valid in a phonemic
-- inventory
listBundles :: PhonemicInventory  -- ^the inventory
               -> Int             -- ^@n@
               -> [FeatureBundle] -- ^all bundles
listBundles pi maxn =
  let allfeats = listFeatures pi
      allns n = do
        x <- replicateM n allfeats
        guard $ length (nub x) == n -- no repet.
        return x
      allupto = [1..maxn] >>= allns
      allsubsets = mapM (\t -> [Feature Plus t, Feature Minus t])
      allvariants = allupto >>= allsubsets
      uniqued = nub $ map (sortBy (compare `on` featureName)) allvariants
  in
   map Bundle uniqued

-- |Selects those bundles for an inventory that pick out a proper non-empty subset
-- of phonemes in that inventory
selectRelevantBundles :: PhonemicInventory   -- ^the inventory
                         -> Int              -- ^maximum @n@ of features in a bundle
                         -> [FeatureBundle]  -- ^the resulting bundle
selectRelevantBundles pi maxn =
  let
    pick b = pickByFeature pi b
    nphonemes = length pi
    relevant bundle = let x = length $ pick bundle
                      in
                       x > 0 && x < nphonemes
    allrelevant = filter relevant (listBundles pi maxn)
    getAllPhonemes bundle = concat (map phonemeName (pick bundle))
    picked :: [FeatureBundle]
    picked = sortBy (compare `on` getAllPhonemes) allrelevant
    grouped :: [[FeatureBundle]]
    grouped = groupBy (\x y -> pick x == pick y) picked
    leastFeatures :: FeatureBundle -> FeatureBundle -> Ordering
    leastFeatures = compare `on` (length . getBundle)
    uniteGroup :: [FeatureBundle] -> FeatureBundle
    uniteGroup  = minimumBy leastFeatures
    united = map uniteGroup grouped
  in
   united

-- |Should do the same as 'selectRelevantBundles'. In a better way!
-- Still shit, 30128 nat.classes? need to do better :(
getNaturalClasses :: PhonemicInventory  -- ^the inventory we start from
                  -> FeatureBundle      -- ^the feature bundle we already have
                  -> [[Phoneme]]        -- ^phoneme sets we've already done
                  -> [FeatureBundle]    -- ^the resulting bundle
getNaturalClasses pi fb dones =
  let
    picked = pickByFeature pi fb: dones
    exploreBundle featName =
      if featName `elem` map featureName (getBundle fb)
      then []
      else let
        newfbp = Bundle $ Feature Plus featName:getBundle fb
        newfbm = Bundle $ Feature Minus featName:getBundle fb
        plusphs  = pickByFeature pi newfbp
        minusphs = pickByFeature pi newfbm
        in
         addAndExplore newfbp plusphs ++ addAndExplore newfbm minusphs
    addAndExplore b p =
      case length p of
        0 -> []
        _ -> if p `elem` picked
             then []
             else b:getNaturalClasses pi b picked
  in
   concatMap exploreBundle (listFeatures pi)

  {-let allpossfeats = listFeatures pi >>=
                                      (`fmap` [Plus, Minus, Null]) . flip Feature
                       allsubsets = do
                         sub <- filterM (const [True, False]) allpossfeats
                         guard (length sub == 1)
                         guard (nub sub == sub)
                         return $ setBundle sub
                   in
                    allsubsets -}

{-
listFeatures pi = let feats = map (getBundle . featureBundle) pi
                      allfeats = join feats
                      allfnames = map featureName allfeats
                      uniqfnames = nub allfnames
                  in
                   uniqfnames
-}



-- |Segment a token, so far only with *digraphs*
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

-- |Inverse of 'segment': creates a Token from phonemes
spellout :: [Phoneme] -> Token
spellout phs = T.pack $ concatMap phonemeName phs



{-
GENERATING
-}

-- |Generates lists of phonemes based on a given pattern built by feature bundles.
-- E.g.
--
-- >>>  map spellout $ generatePattern finnishInventory [labial,velar]
--
generateFromFBs :: PhonemicInventory -> [FeatureBundle] -> [[Phoneme]]
generateFromFBs pi (fb:[]) = map (\x->[x]) $ filterInventoryByBundle pi fb
generateFromFBs pi (fb:fbs) =
  let
    x = generateFromFBs pi fbs
    myphonemes = filterInventoryByBundle pi fb
  in
    [ b:a | a <- x, b <- myphonemes]


-- |Augment a word with a word boundary on either edge
augmentWord :: [Phoneme] -- ^The original, non-augmented word
               -> (Maybe Phoneme, Maybe Phoneme) -- ^A tuple with the boundaries. If you don't want to augment on either side, just pass Nothing here
               -> [Phoneme] -- ^The resulting word
augmentWord wd (left, right) = aug left ++ wd ++ aug right
  where
    aug bound = case bound of
      Nothing -> []
      Just x -> [x]


segmentWords :: PhonemicInventory -> [Token] -> Writer Txt.Text ([Maybe [Phoneme]])
segmentWords pi tokens = mapM (\tok -> do
                                  let seg = segment pi tok
                                  when (isNothing seg) $ tell (T.getText tok <> "\n")
                                  return seg
                              ) tokens

