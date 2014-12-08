import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hanalyze.FreqDist
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

testLoading ::  IO Bool
testLoading = do
  toks <- readCountFreqs "testfile"
  let map = getMap toks
      tests = HU.test [
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
  cnts <- HU.runTestTT tests
  let errs = HU.errors cnts + HU.failures cnts
  return (errs == 0)

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

main :: IO ()
main = do
  putStrLn "Testing."
  myCheck "Monoid law I: " prop_monoid_i
  myCheck "Monoid law II: " prop_monoid_ii
  myCheck "Monoid law III: " prop_monoid_iii
  myCheck "Monoid law IV: " prop_monoid_iv
  testLoading >>= flip unless exitFailure
  -- doesn't work because laziness :/ writing does not start before reading
  -- myCheck "Saving and loading: " prop_saveLoad
  testFilter >>= flip unless exitFailure
  exitSuccess
  
