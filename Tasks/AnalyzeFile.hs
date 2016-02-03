module Tasks.AnalyzeFile (task) where

import           Control.Exception
import           Hanalyze.Chisq
import           Hanalyze.FreqDist
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import qualified Hanalyze.Token as T
import           Hanalyze.Vowels
import           Tasks.Summaries (summarySection, getTypeFreq, vowelSummarySection)
import           Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "analyzefile"
         "The first analysis task from way back in early 2015. Does chi square tests yay!"

doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
      maxn = case getFlag flags MaxN of
        Nothing -> 2
        Just (MaxN (IntParam i)) -> i
  infn <- dieIfNoFN minfn
  fd <- readFreqDist infn
  summarySection fd
  -- get relevant bundles for vowels
  let vowels = filterInventoryByBundle finnishInventory vowel
      vowelRelevants = selectRelevantBundles vowels maxn
      fd' = filterTable (filterToken finnishInventory [StarF consonant, DotF vowel, StarF consonant, StarF vowel]) fd
      patternGenerator fb = [StarF consonant, DotF fb, StarF consonant, StarF vowel]
      patterns = map patternGenerator vowelRelevants
  mapM_ (summarizeByPattern fd' finnishInventory) patterns

summarizeByPattern :: FreqDist -> PhonemicInventory -> [Pattern] -> IO AnnotatedFreqDist
summarizeByPattern fd inv patt = do
  let funFits = filterToken inv patt
      funDoesntfit = not . funFits
      fdFits = filterTable funFits fd
      fdDoesntfit = filterTable funDoesntfit fd
  putStrLn $ " ========*** " ++ writePattern patt ++ " ***========"
  fitMap       <- vowelSummarySection ("fitting pattern " ++ writePattern patt) fdFits harmonicity
  doesntfitMap <- vowelSummarySection ("not fitting pattern " ++ writePattern patt) fdDoesntfit harmonicity
  let getTypeFreqs m = map (getTypeFreq . snd) (tToList m)
      table = [getTypeFreqs fitMap, getTypeFreqs doesntfitMap]
  putStrLn $ show table
  catch (do
            let xsqtest = runChiSqTest table True
            putStrLn $ "\nChi Sq = " ++ show (chisq xsqtest) ++ ", p = " ++ show (p xsqtest) ++ ", " ++ if sig xsqtest then "*SIGNIFICANT*" else "n.s."
        ) (\err -> putStrLn $"No chi square test available, " ++ (show (err::SomeException)))
  return $ annotateFD [(T.pack ("fits_"++writePattern patt), funFits),
                       (T.pack ("fitsnot_"++writePattern patt), funDoesntfit)] fd

