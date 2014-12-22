{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Omorfi
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Hanalyze.Token as T
import Data.Monoid
import qualified Test.HUnit as HU
import Test.HUnit ((~=?), (~:), (@?))
import Control.Monad (unless)
import System.Exit
import System.Process
import Control.Concurrent
import qualified Data.List as List (foldl')

instance Arbitrary FreqDist where
  arbitrary = do
    positives <- arbitrary 
    names <- arbitrary
    return $ FreqDist $  Map.fromList $  zip names (map abs positives)
    

instance Arbitrary Token where
  arbitrary =  T.pack <$> arbitrary

prop_monoid_i :: FreqDist -> Bool
prop_monoid_i fd = mappend mempty fd == fd

prop_monoid_ii :: FreqDist -> Bool
prop_monoid_ii fd = mappend fd mempty == fd

prop_monoid_iii :: FreqDist -> FreqDist -> FreqDist -> Bool
prop_monoid_iii x y z = mappend x (mappend y z) == mappend (mappend x y) z

prop_monoid_iv :: [FreqDist] -> Bool
prop_monoid_iv fds = mconcat fds == foldr mappend mempty fds


huTest :: HU.Testable t => t -> IO Bool
huTest tests = do
  cnts <- HU.runTestTT $ HU.test tests
  let errs = HU.errors cnts + HU.failures cnts
  return (errs == 0)

testLoading ::  IO Bool
testLoading = do
  toks <- readCountFreqs "testfile"
  let map = getMap toks
  huTest [
        "simple read" ~: do
           Map.lookup (T.pack "alma") map /= Nothing @? "Finding alma"
           Map.lookup (T.pack "karaj") map /= Nothing @? "Finding karaj"
           Map.lookup (T.pack "nonsense") map == Nothing @? "Not finding nonsense"
        ,
        "unicode reading" ~: do
           Map.lookup (T.pack "hyppää") map /= Nothing @? "Finding hyppää"
           Map.lookup (T.pack "❤") map /= Nothing @? "Finding ❤"
        ,
        "counting" ~: do
           Map.lookup (T.pack "alma") map == Just 2 @? "Counting simple addition"
           Map.lookup (T.pack "kke") map == Just 1 @? "Counting one"
           Map.lookup (T.pack "karaj") map == Just 2 @? "Counting lowercase"
        ]

testHarmony :: IO Bool
testHarmony = do
  huTest [
        "harmonyV" ~: do
           harmonyV 'a' == Just Back @? "a"
           harmonyV 'ä' == Just Front @? "ä"
           harmonyV 'e' == Just Neutral @? "e"
        ,
        "harmonicity" ~: do
           harmonicity "qalmaq" == AllBack @? "AllBack"
           harmonicity "qälmäq" == AllFront @? "AllFront" 
           harmonicity "qälmaq" == FrontBack @? "FrontBack" 
           harmonicity "qelmiq" == AllNeutral @? "AllNeutral" 
           harmonicity "qelmäq" == FrontNeutral @? "FrontNeutral" 
           harmonicity "qelmaq" == BackNeutral @? "BackNeutral" 
           harmonicity "qalmeq" == BackNeutral @? "BackNeutral" 
           harmonicity "qalmäq" == FrontBack @? "FrontBack" 
           harmonicity "qälmeq" == FrontNeutral @? "FrontNeutral" 
           harmonicity "fdsqplw" == Anything @? "Anything"
        ,
        "fullHarmonic" ~: do
           fullHarmonic "xalmax" Back @? "all back"
           (not $ fullHarmonic "xalmex"  Back) @? "mixed"
           fullHarmonic "xälmäz" Front @? "all front"
           fullHarmonic "xelmex" Neutral @? "all neutral"
        ,
        "suffixIt" ~: do
           suffixIt AllFront == FrontSuffixes @? "all front"
           suffixIt AllBack == BackSuffixes @? "all back"
           suffixIt BackNeutral == BackSuffixes @? "back neutral"           
           suffixIt FrontNeutral == FrontSuffixes @? "front neutral"           
           suffixIt FrontBack == LastVowel @? "front back"     
           suffixIt AllNeutral == FrontSuffixes @? "all neutral"
        ,
        "onlyVowels" ~: do
           onlyVowels "alma" == "aa" @? "aa fine"
           onlyVowels "almä" == "aä" @? "aä fine"
           onlyVowels "almö" == "aö" @? "aö fine"
        ]


-- multiread?
  
saveLoad :: FreqDist -> FilePath -> IO FreqDist
saveLoad fd fn = do
  saveTable fd fn
  readFreqDist fn

prop_saveLoad :: FreqDist -> Property
prop_saveLoad fd = monadicIO $ do
                       mv <- run $ newMVar False
                       run $ forkFinally (saveTable fd "temp.tmp.tmp")
                           (\_ -> putMVar mv True)
                       x <- run $ takeMVar mv
                       fd2 <- run $ readFreqDist "temp.tmp.tmp"
                       assert (fd == fd2 && x)
                   

testFilter :: IO Bool
testFilter =
  runCommand "dist/build/filter_fds/filter_fds freqdists/freqdist_xaaa" >>=
  waitForProcess >>
  runCommand "diff -q freqdists/filtered_freqdist_xaaa freqdists/filtered_freqdist_xaaa_testagainst" >>=
  waitForProcess >>= \excode ->
  if excode == ExitSuccess then
    return True
  else
    return False

testOmorfiPlain :: IO Bool
testOmorfiPlain = do
  ofd <- loadOmorfiFile "freqdists/omorfitest_analyzed"
  let ofdmap = tGetMap ofd
  huTest [
    "tokennames" ~: do
       "hilkan" `elem` Map.keys ofdmap @? "token hilkan not found"
       "hiljan" `elem` Map.keys ofdmap @? "token hiljan not found"
       "hiljaa" `elem` Map.keys ofdmap @? "token hiljaa not found"
    ,
    "poses" ~: do
       getPOS ((ofdmap ! "hilkan") !! 0) == N @? "hilkan N"
       getPOS ((ofdmap ! "hiljan") !! 0) == Other @? "hiljan Adv"
       getPOS ((ofdmap ! "hiljaa") !! 0) == Other @? "hiljaa Adv"
    ,
    "stems" ~: do
       getStem ((ofdmap ! "hilkan") !! 0) == "hilkka" @? "hilkan stem"
       getStem ((ofdmap ! "hiljan") !! 0) == "hiljan" @? "hiljan stem"
       getStem ((ofdmap ! "hiljaa") !! 0) == "hiljaa" @? "hiljaa stem"
    ,
    "ois" ~: do
       getOtherInfo ((ofdmap ! "hilkan") !! 0) == OtherInfo "Gen Sg" @? "hilkan oi"
       getOtherInfo ((ofdmap ! "hiljan") !! 0) == NoOI @? "hiljan oi"
       getOtherInfo ((ofdmap ! "hiljaa") !! 0) == NoOI @? "hiljaa oi"
    ]
                  

prop_sum fd = sumFD fd == List.foldl' (+) 0 (map snd (Map.toList $ getMap fd))


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
  putStrLn "Testing."
  myCheck "Monoid law I: " prop_monoid_i
  myCheck "Monoid law II: " prop_monoid_ii
  myCheck "Monoid law III: " prop_monoid_iii
  myCheck "Monoid law IV: " prop_monoid_iv
  testLoading >>= flip unless (giveUp "testLoading")
  -- doesn't work because laziness :/ writing does not start before reading
  -- myCheck "Saving and loading: " prop_saveLoad
  testFilter >>= flip unless (giveUp "testLoading")
  testHarmony >>= flip unless (giveUp "testHarmonyW")
  myCheck "Summing FDs: " prop_sum
  testOmorfiPlain >>= flip unless (giveUp "testOmorfiPlain")
  exitSuccess
  
