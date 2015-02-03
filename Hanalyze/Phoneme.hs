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
         mergeFeature, mergeFeature', mergeBundle, subsetFB,

         -- * Lookup functions
         findInBundle, findPhoneme,

         -- * Segmenting
         segment,

                       
         -- * Feature system
         -- ** basic features
         -- *** consonants
         fLabial, fCoronal, fVelar, fGlottal, fPalatal, fContinuant,
         fSonorant, fCons, fVoiced, fVoiceless,

         -- *** vowels
         fVowel, fHigh, fLow,
         fRounded, fFront, fLong,

         -- ** as feature bundles
         -- *** consonants
         labial, coronal, velar, glottal, palatal, voiced, voiceless,
         stop, nasal, fricative, approximant, lateral, trill, short,
         long, consonant,

         -- *** vowels
         high, mid, low, rounded, unrounded, front, back, vowel,

         -- * Inventories
         finnishInventory, selectRelevantBundles, listFeatures,
         listBundles, pickByFeature, filterInventory,
         filterInventoryByBundle

         ) where

import Data.Monoid
import Data.Maybe
import Control.Monad
import qualified Hanalyze.Token as T
import Hanalyze.Token (Token)
import Data.List (delete,nub,sortBy,minimumBy,groupBy)
import Data.Function (on)

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
               
-- |Merges two bundles so that there is no duplication. It might fail if a
-- feature with an empty string name  is used -- please don't do that!
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

fContinuant = Feature Plus "continuant"
fSonorant =  Feature Plus "sonorant"
fCons = Feature Plus "consonantal"
fVowel = minus fCons

fVoiced = Feature Plus "voiced"
fVoiceless = Feature Minus "voiced"

fHigh = Feature Plus "high"
fLow = Feature Plus "low"
fRounded = Feature Plus "rounded"

fFront = Feature Plus "front"
fLong = Feature Plus "long"


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

lateral = Bundle [Feature Plus "lateral"]
trill = Bundle [Feature Plus "trill"]

short = Bundle [minus fLong]
long = Bundle [fLong]
consonant = Bundle [fCons]

-- vowels
vowel = Bundle [fVowel]
high = Bundle [fHigh, minus fLow, fVowel]
mid = Bundle [minus fHigh, minus fLow, fVowel]
low = Bundle [minus fHigh, fLow, fVowel]

rounded = Bundle [fRounded, fVowel]
unrounded = Bundle [minus fRounded, fVowel]

front = Bundle [fFront, fVowel]
back = Bundle [minus fFront, fVowel]

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


-- |Produces more or less the relevant Finnish phonemic inventory.
finnishInventory :: PhonemicInventory
finnishInventory = mapWith testInv short ++ mapWith doubledInv long ++ [
  Phoneme "ie" (mconcat [front, high, unrounded, long]),
  Phoneme "ei" (mconcat [front, mid, unrounded, long])
  ]
  where
    mapWith inv feat = map (\phon -> Phoneme (phonemeName phon) (mconcat [featureBundle phon, feat]) ) inv
    doubledInv = map (\phon -> let n = phonemeName phon in
                       Phoneme (n ++ if n == "ng" then "" else n) (featureBundle phon)) testInv

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

