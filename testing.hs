import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hanalyze.FreqDist
import Hanalyze.Vowels
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Monoid
import qualified Test.HUnit as HU
import Test.HUnit ((~=?), (~:), (@?))
import Control.Monad (unless)
import System.Exit
import System.Process
import Control.Concurrent

instance Arbitrary FreqDist where
  arbitrary = do
    positives <- arbitrary 
    names <- arbitrary
    return $ FreqDist $  Map.fromList $  zip names (map abs positives)
    

instance Arbitrary T.Text where
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
        ]


-- multiread?
  
saveLoad :: FreqDist -> FilePath -> IO FreqDist
saveLoad fd fn = do
  saveFreqDist fd fn
  readFreqDist fn

prop_saveLoad :: FreqDist -> Property
prop_saveLoad fd = monadicIO $ do
                       mv <- run $ newMVar False
                       run $ forkFinally (saveFreqDist fd "temp.tmp.tmp")
                           (\_ -> putMVar mv True)
                       x <- run $ takeMVar mv
                       fd2 <- run $ readFreqDist "temp.tmp.tmp"
                       assert (fd == fd2 && x)
                   

testFilter :: IO Bool
testFilter =
  runCommand "dist/build/filter_fds/filter_fds freqdist_xaaa" >>=
  waitForProcess >>
  runCommand "diff -q filtered_freqdist_xaaa filtered_freqdist_xaaa_testagainst" >>=
  waitForProcess >>= \excode ->
  if excode == ExitSuccess then
    return True
  else
    return False



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
  exitSuccess
  
