{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hanalyze.Phoneme
import Hanalyze.Pattern
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

instance Arbitrary PlusMinus where
  arbitrary = elements [Plus, Minus, Null]

instance Arbitrary Feature where
  arbitrary = liftM2 F arbitrary arbitrary

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
       F Plus "alma" `mergeFeature'` F Minus "alma" == Just (F Null "alma") @? "+ - = 0"
       F Minus "alma" `mergeFeature'` F Minus "alma" == Just (F Minus "alma") @? "- - = -"
       F Plus "alma" `mergeFeature'` F Plus "alma" == Just (F Plus "alma") @? "+ + = +"
       F Plus "alma" `mergeFeature'` F Null "alma" == Just (F Plus "alma") @? "+ 0 = +"
       F Null "alma" `mergeFeature'` F Minus "alma" == Just (F Minus "alma") @? "0 - = 0-"
       F Plus "elme" `mergeFeature'` F Minus "alma" == Nothing @? "+ - = 0"
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
  exitSuccess
  
