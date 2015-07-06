{-# LANGUAGE OverloadedStrings #-}
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Hanalyze.Omorfi
import Hanalyze.Chisq
import Hanalyze.Phoneme
import Hanalyze.Pattern
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import Data.Map ((!),size,member)
import qualified Hanalyze.Token as T
import Data.Monoid
import Data.Maybe
import qualified Test.HUnit as HU
import Test.HUnit ((~=?), (~:), (@?))
import Control.Monad 
import System.Exit
import System.Process
import System.Posix.Directory
import Control.Concurrent
import qualified Data.List as List (foldl')

instance Arbitrary FreqDist where
  arbitrary = do
    positives <- arbitrary 
    names <- arbitrary
    return $ FreqDist $  Map.fromList $  zip names (map abs positives)
    

instance Arbitrary Token where
  arbitrary =  T.pack <$> arbitrary



instance Arbitrary PlusMinus where
  arbitrary = elements [Plus, Minus, Null]

instance Arbitrary Feature where
  arbitrary = liftM2 Feature arbitrary arbitrary

instance Arbitrary FeatureBundle where
  arbitrary = liftM setBundle arbitrary


prop_monoid_i :: FreqDist -> Bool
prop_monoid_i fd = mappend mempty fd == fd

prop_monoid_ii :: FreqDist -> Bool
prop_monoid_ii fd = mappend fd mempty == fd

prop_monoid_iii :: FreqDist -> FreqDist -> FreqDist -> Bool
prop_monoid_iii x y z = mappend x (mappend y z) == mappend (mappend x y) z

prop_monoid_iv :: [FreqDist] -> Bool
prop_monoid_iv fds = mconcat fds == foldr mappend mempty fds


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
testHarmony =
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
           not (fullHarmonic "xalmex"  Back) @? "mixed"
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
  runCommand "../dist/build/filter_fds/filter_fds freqdist_xaaa" >>=
  waitForProcess >>
  runCommand "diff -q filtered_freqdist_xaaa filtered_freqdist_xaaa_testagainst" >>=
  waitForProcess >>= \excode ->
  if excode == ExitSuccess then
    return True
  else
    return False

testOmorfiPlain :: IO Bool
testOmorfiPlain = do
  ofd <- loadOmorfiFile "omorfitest_analyzed"
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
                  
testOmorfi :: IO Bool
testOmorfi = do
  ofd <- readFreqDist "omorfitest2"
  om <- analyseFDOmorfi ofd
  let ofdmap = tGetMap om
  huTest [
    "tokennames" ~: do
       "joulu" `elem` Map.keys ofdmap @? "token joulu not found"
       "joulun" `elem` Map.keys ofdmap @? "token joulun not found"
       "pitkä" `elem` Map.keys ofdmap @? "token pitkä not found"
       "vitsa" `elem` Map.keys ofdmap @? "token vitsa not found"
    ,
    "stems" ~: do
       getStem ((ofdmap ! "joulun") !! 0) == "joulu" @? "joulun stem"
       getStem ((ofdmap ! "pitkä") !! 0) == "pitkä" @? "pitkä stem"
       getStem ((ofdmap ! "vitsa") !! 0) == "vitsa" @? "vitsa stem"
    ]

prop_sum fd = sumTable fd == List.foldl' (+) 0 (map snd (Map.toList $ getMap fd))

-- prop_ffi :: Int -> Bool
-- prop_ffi int = (mytest int) == int + 1

testChisq :: IO Bool
testChisq = do
  let table = [[12.0,7.0],[5.0,7.0]]
      mchisq = getChiSq table True
      chitest = runChiSqTest table True
  huTest [
    "chi^2" ~: do
       mchisq > 0.64 && mchisq < 0.65 @? ("chisq is not 0.64--0.65, it is" ++ (show mchisq))
       chisq chitest == mchisq @? "chisq accessor"
       p chitest > 0.576 && p chitest < 0.577 @? ("p is not 0.576--0.577, it is" ++ (show $ p chitest))
       not (sig chitest) @? "significance"
    ]

{-
prop_monoid_i :: FreqDist -> Bool
prop_monoid_i fd = mappend mempty fd == fd
-}

huTest :: HU.Testable t => t -> IO Bool
huTest tests = do
  cnts <- HU.runTestTT $ HU.test tests
  let errs = HU.errors cnts + HU.failures cnts
  return (errs == 0)


testFeatureAddition = 
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
       phonemeName (fromJust (segment finnishInventory "keke") !! 3) == "e" @? "simple lookup"
       phonemeName (fromJust (segment finnishInventory "aalassa") !! 3) == "ss" @? "long segment lookups"
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
      filteredFd pat = filterTable (filterToken finnishInventory pat) fd
      filteredMap pat = getMap $ filteredFd pat
      ppattern = readPattern finnishInventory
  in
   putStrLn ("Here we go: " ++ show (filteredMap patternSoph2)) >>
   huTest [
     "pattern reading" ~: do
        pattern == (fromJust $ ppattern "p*p") @? "reading pattern 1"
        pattern2 == (fromJust $ ppattern "*p*") @? "reading pattern 2"
        pattern3 == (fromJust $ ppattern "*p?") @? "reading pattern 3"
        pattern4 == (fromJust $ ppattern "*p") @? "reading pattern 4"
        pattern5 == (fromJust $ ppattern "*p.") @? "reading pattern 5"
        patternSoph3 == (fromJust $ ppattern "[a,b]*") @? "reading sophisticated pattern 3"
     ,
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

testMatchme :: IO Bool
testMatchme =
  let testmatch str patt = 
        let testWord = case segment finnishInventory (T.pack str) of
              Just x -> x
              _ -> []
      -- Star = matches none if nothing left
            p = case readPattern finnishInventory patt of
              Nothing -> error "pattern reading fail"
              Just x -> x
            res = case matchWord testWord p of
              Just phons -> T.unpack $ spellout phons
              Nothing -> "no match"
        in
         res
  in 
   huTest [
     "consonants" ~: do
        testmatch "labda" "{+consonantal}*" == "l" @? "l-abda"
        testmatch "lkrabda" "{+consonantal}*" == "lkr" @? "lkr-abda"
        testmatch "abda" "{+consonantal}*" == "no match" @? "0-abda"
     ,
     "until first V no diphthongs" ~: do
       testmatch "akia" "{+consonantal}*{-consonantal}.{+consonantal}*" == "ak" @? "akia"
       testmatch "kala" "{+consonantal}*{-consonantal}.{+consonantal}*" == "kal" @? "kala"
       testmatch "sprispra" "{+consonantal}*{-consonantal}.{+consonantal}*" == "sprispr" @? "sprispra"
       testmatch "aikia" "{+consonantal}*{-consonantal}.{+consonantal}*" == "a" @? "aikia"
     ,
     "until first V yes diphthongs" ~: do
       testmatch "aikia" "{+consonantal}*{-consonantal}.{-consonantal}*{+consonantal}*" == "aik" @? "aikia"
       testmatch "kaila" "{+consonantal}*{-consonantal}.{-consonantal}*{+consonantal}*" == "kail" @? "kaila"
       testmatch "sprispra" "{+consonantal}*{-consonantal}.{-consonantal}*{+consonantal}*" == "sprispr" @? "sprispra"
    ]



myCheck :: (Testable prop) => String -> prop -> IO ()
myCheck s pr = putStr s >> quickCheckResult pr >>= \res ->
  case res of
    Success {}  -> return ()
    _ -> exitFailure

giveUp :: String -> IO a
giveUp s = putStrLn ("Error with " ++ s) >>
           exitFailure

existsCommand :: String -> IO Bool
existsCommand com = do
  (ec,so,se) <- readProcessWithExitCode "which" [com] ""
  return (so /= "")

runIfExistsCommand :: String -> IO () -> IO ()
runIfExistsCommand com io = do
  ec <- existsCommand com
  when ec io

main :: IO ()
main = do
  changeWorkingDirectory "tests"
  runCommand "pwd"
  putStrLn "Testing."
  myCheck "Monoid law I: " prop_monoid_i
  myCheck "Monoid law II: " prop_monoid_ii
  myCheck "Monoid law III: " prop_monoid_iii
  myCheck "Monoid law IV: " prop_monoid_iv
--  myCheck "ffi" prop_ffi
  testChisq  >>= flip unless (giveUp "chiSq")
  testLoading >>= flip unless (giveUp "testLoading")
  -- doesn't work because laziness :/ writing does not start before reading
  -- myCheck "Saving and loading: " prop_saveLoad
  testFilter >>= flip unless (giveUp "testLoading")
  testHarmony >>= flip unless (giveUp "testHarmonyW")
  myCheck "Summing FDs: " prop_sum
  testOmorfiPlain >>= flip unless (giveUp "testOmorfiPlain")
  runIfExistsCommand  "omorfi-interactive.sh" (testOmorfi >>= flip unless (giveUp "testOmorfiPlain"))
-- ex-testing_phonemes
  putStrLn "Testing phonemes."
  testFeatureAddition >>= flip unless (giveUp "testFeatureAddition")
  myCheck "Finding features" prop_finding
  myCheck "Merging bundles" prop_merging
  testFinnish >>= flip unless (giveUp "finnish inventory")
  myCheck "Patterns" prop_patterns
  testFilterme >>= flip unless (giveUp "finnish inventory")
  exitSuccess
  
