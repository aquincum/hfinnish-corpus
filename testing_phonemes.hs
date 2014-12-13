{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hanalyze.Phoneme
import Hanalyze.Pattern
import Hanalyze.FreqDist
import qualified Hanalyze.Token as T
import Data.Monoid
import Data.Maybe
import qualified Test.HUnit as HU
import Test.HUnit ((~=?), (~:), (@?))
import Control.Monad
import System.Exit
import System.Process
import Control.Concurrent
import qualified Data.List as List (foldl')
import Data.Map (size,(!),member)

instance Arbitrary PlusMinus where
  arbitrary = elements [Plus, Minus, Null]

instance Arbitrary Feature where
  arbitrary = liftM2 Feature arbitrary arbitrary

instance Arbitrary FeatureBundle where
  arbitrary = liftM setBundle arbitrary

{-
prop_monoid_i :: FreqDist -> Bool
prop_monoid_i fd = mappend mempty fd == fd
-}

huTest :: HU.Testable t => t -> IO Bool
huTest tests = do
  cnts <- HU.runTestTT $ HU.test tests
  let errs = HU.errors cnts + HU.failures cnts
  return (errs == 0)


testFeatureAddition = do
  huTest [
    "mergeFeature'" ~: do
       Feature Plus "alma" `mergeFeature'` Feature Minus "alma" == Just (Feature Null "alma") @? "+ - = 0"
       Feature Minus "alma" `mergeFeature'` Feature Minus "alma" == Just (Feature Minus "alma") @? "- - = -"
       Feature Plus "alma" `mergeFeature'` Feature Plus "alma" == Just (Feature Plus "alma") @? "+ + = +"
       Feature Plus "alma" `mergeFeature'` Feature Null "alma" == Just (Feature Plus "alma") @? "+ 0 = +"
       Feature Null "alma" `mergeFeature'` Feature Minus "alma" == Just (Feature Minus "alma") @? "0 - = 0-"
       Feature Plus "elme" `mergeFeature'` Feature Minus "alma" == Nothing @? "+ - = 0"
    ]

prop_finding fb = let firstF = featureName $ head $ getBundle fb
                      lastF = featureName $ last $ getBundle fb
                      foundFF = findInBundle firstF fb
                      foundLF = findInBundle lastF fb
                  in
                   (getBundle fb /= []) ==>
                   isJust foundFF && isJust foundLF && featureName  (fromJust foundFF) == firstF && featureName (fromJust foundLF) == lastF

prop_merging fb1 fb2 =
  let
    feats1 = getBundle fb1
    merged = mergeBundle fb1 fb2
  in
   (getBundle fb1 /= []) ==>
   (getBundle fb2 /= []) ==>
   forAll (elements feats1) (\feat -> featureName feat /= "" ==>
                              case findInBundle (featureName feat) merged of
                                Nothing -> False
                                Just mfeat | featureName mfeat /= featureName feat -> False
                                           | plusMinus mfeat /= plusMinus feat -> plusMinus mfeat == Null && isJust (findInBundle (featureName feat) fb2)
                                           | otherwise -> True

                              
                            )
  

testFinnish :: IO Bool
testFinnish = 
  huTest [
    "segmenting" ~: do
       length(fromJust $ segment finnishInventory "alma") == 4 @? "simple length"
       phonemeName ((fromJust $ segment finnishInventory "keke") !! 3) == "e" @? "simple lookup"
       phonemeName ((fromJust $ segment finnishInventory "aalassa") !! 3) == "ss" @? "long segment lookups"
       isNothing (segment finnishInventory "jëlla") @? "returning nothing"
  ]

prop_patterns str = case readPattern finnishInventory (T.pack str) of
  Nothing -> True
  Just x -> writePattern x == str

testFilterme :: IO Bool 
testFilterme = 
  let corpus = T.words $ T.pack "allma belme apää pep pepe belme apää allma allma"
      fd = countFreqs corpus
      phonP = fromJust $ findPhoneme finnishInventory "p"
      phonA = fromJust $ findPhoneme finnishInventory "a"
      phonB = fromJust $ findPhoneme finnishInventory "b"
      pattern = [P phonP, Star, P phonP]
      pattern2 = [Star, P phonP, Star]
      pattern3 = [Star, P phonP, Question]
      pattern4 = [Star, P phonP]
      pattern5 = [Star, P phonP, Dot]
      patternSoph1 = [Star, DotF $ mconcat [labial, nasal] , Star]
      patternSoph2 = [QuestionF $ mconcat [labial], StarF $ mconcat [vowel], Star]
      patternSoph3 = [AnyP [phonA,phonB], Star]
      filteredFd pat = filterFD (filterToken finnishInventory pat) fd
      filteredMap pat = getMap $ filteredFd pat
  in
   putStrLn ("Here we go: " ++ show (filteredMap patternSoph2)) >>
   huTest [
     "p*p" ~: do
        size (filteredMap pattern) == 1 @? "size " ++ show (filteredMap pattern)
        member "pep" (filteredMap pattern) @? "found " ++ show (filteredMap pattern)
     ,
     "*p*" ~: do
        size (filteredMap pattern2) == 3 @? "size " ++ show (filteredMap pattern2)
        member "pep" (filteredMap pattern2) @? "found pep " ++ show (filteredMap pattern2)
        member "apää" (filteredMap pattern2) @? "found apää " ++ show (filteredMap pattern2)
        member "pepe" (filteredMap pattern2) @? "found pepe " ++ show (filteredMap pattern2)
     ,
     "*p?" ~: do
        size (filteredMap pattern3) == 3 @? "size " ++ show (filteredMap pattern3)
        member "pep" (filteredMap pattern3) @? "found pep " ++ show (filteredMap pattern3)
        member "pepe" (filteredMap pattern3) @? "found pepe " ++ show (filteredMap pattern3)
        member "apää" (filteredMap pattern3) @? "found apää " ++ show (filteredMap pattern3)
     ,
     "*p" ~: do
        size (filteredMap pattern4) == 1 @? "size " ++ show (filteredMap pattern4)
        member "pep" (filteredMap pattern4) @? "found pep " ++ show (filteredMap pattern4)
     ,
     "*p." ~: do
        size (filteredMap pattern5) == 2 @? "size " ++ show (filteredMap pattern5)
        member "pepe" (filteredMap pattern5) @? "found pepe " ++ show (filteredMap pattern5)
        member "apää" (filteredMap pattern5) @? "found apää" ++ show (filteredMap pattern5)
     ,
     "*[lab,nas]*" ~: do
        size (filteredMap patternSoph1) == 2 @? "size " ++ show (filteredMap patternSoph1)
        member "allma" (filteredMap patternSoph1) @? "found allma " ++ show (filteredMap patternSoph1)
        member "belme" (filteredMap patternSoph1) @? "found belme " ++ show (filteredMap patternSoph1)
     ,
     "[lab]?[vow]*.*" ~: do 
        size (filteredMap patternSoph2) == 5 @? "size " ++ show (filteredMap patternSoph2)
        member "pep" (filteredMap patternSoph2) @? "found pep " ++ show (filteredMap patternSoph2)
        member "pepe" (filteredMap patternSoph2) @? "found pepe " ++ show (filteredMap patternSoph2)        
        member "belme" (filteredMap patternSoph2) @? "found belme " ++ show (filteredMap patternSoph2)
        member "allma" (filteredMap patternSoph2) @? "found allma " ++ show (filteredMap patternSoph2)
        member "apää" (filteredMap patternSoph2) @? "found apää" ++ show (filteredMap patternSoph2)
     ,
     "[ab].*" ~: do
       size (filteredMap patternSoph3) == 3 @?  "size " ++ show (filteredMap patternSoph3)
       member "allma" (filteredMap patternSoph2) @? "found allma " ++ show (filteredMap patternSoph3)
       member "apää" (filteredMap patternSoph2) @? "found apää " ++ show (filteredMap patternSoph3)
       member "belme" (filteredMap patternSoph2) @? "found belme " ++ show (filteredMap patternSoph3)        
     ]
       



myCheck :: (Testable prop) => String -> prop -> IO ()
myCheck s pr = putStr s >> quickCheckResult pr >>= \res ->
  case res of
    Success {}  -> return ()
    _ -> exitFailure

giveUp :: String -> IO a
giveUp s = putStrLn ("Error with " ++ s) >>
           exitFailure

main :: IO ()
main = do
  putStrLn "Testing phonemes."
  testFeatureAddition >>= flip unless (giveUp "testFeatureAddition")
  myCheck "Finding features" prop_finding
  myCheck "Merging bundles" prop_merging
  testFinnish >>= flip unless (giveUp "finnish inventory")
  myCheck "Patterns" prop_patterns
  testFilterme >>= flip unless (giveUp "finnish inventory")
  exitSuccess
  
