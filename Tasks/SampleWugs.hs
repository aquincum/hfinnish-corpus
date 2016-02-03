module Tasks.SampleWugs (task) where

import           Data.Maybe
import           Data.Monoid
import           Data.Random
import           Data.Random.Source.DevRandom
import qualified Data.Text as Txt
import qualified Data.Text.IO as TIO
import           Hanalyze.FreqDist
import           Hanalyze.Omorfi
import           Hanalyze.Pattern
import           Hanalyze.Phoneme
import           Hanalyze.ToUCLAP
import qualified Hanalyze.Token as T
import           System.IO
import           Tasks.Task

task :: Task
task = Task
         doTask
         (Just FileName)
         "samplewugs"
         "Samples wugs for the experiment in chapter 2. Input is a blickTestResults.txt from an UCLAPL run, where all the possible wugs are tested. Only 0 scores and unkonow word pairs are included. "


doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
      samplenone = case getFlag flags SampleNone of
        Nothing -> error "No --samplenone argument given."
        Just (SampleNone (IntParam n)) -> n
      samplepatt = case getFlag flags SamplePatt of
        Nothing -> error "No --samplepatt argument given."
        Just (SamplePatt (IntParam n)) -> n
  infn <- dieIfNoFN minfn
  contents <- TIO.readFile infn
  putStrLn "File read."
  let cWithAe = Txt.intercalate (Txt.pack "ä") (Txt.splitOn (Txt.pack "ae") contents)
      cWithOe = Txt.intercalate (Txt.pack "ö") (Txt.splitOn (Txt.pack "oe") cWithAe)
      allWugs = readUCLAPLOutput cWithOe infn
  putStrLn $ "Umlauts reitroduced. n = " ++ show (tSize allWugs)
  let zeroWeight = filterByValTable (== 0.0) allWugs
      zeroWeightFD = (tFromList $ map (\(t,_) -> (t,0)) (tToList zeroWeight)) :: FreqDist
  putStrLn $ "Zero weights filtered. n = " ++ show (tSize zeroWeightFD)
  let noSpacesFD = mapTable (T.filter (/= ' ')) zeroWeightFD
  putStrLn $ "Spaces removed. n = " ++ show (tSize noSpacesFD)
  morphanalyzed <- analyseFDOmorfi noSpacesFD
  putStrLn $ "Morphological parsing done. n = " ++ show (tSize morphanalyzed)
  let unknownsFD = takeStems $ getUnknownWords morphanalyzed
  putStrLn $ "Known words filtered. n = " ++ show (tSize unknownsFD)
  let unknownPairsFD = findCouples unknownsFD
  putStrLn $ "Unknown pairs found. n = " ++ show (tSize unknownPairsFD)
  let annotated = annotateWithPatterns unknownPairsFD 
  putStrLn $ "Annotation done. Sampling..."
  let createSample :: Int -> String -> IO AnnotatedFreqDist
      createSample n annot =
        let
          part = tToList $ filterWithAnnotation (==T.pack annot) annotated
        in
         runRVar (shuffleNofM (min n (length part)) (length part) part) DevURandom >>=
         return . tFromList
  nonesSample <- createSample samplenone ""
  pattSample <- mapM (createSample samplepatt) ["1","2","3","4","5"]
  putStrLn $ "Sampling done."
  withFile "sampled-wugs.txt" WriteMode $ \h ->
    writeTable nonesSample h >>
    mapM_ (flip writeTable h) pattSample

-- |Keep only stems that have a pair: ending in -a ~ ending in -ä
findCouples :: FreqDist -> FreqDist
findCouples fd =
  let
    revfd = mapTable T.reverse fd
    getHead t = case T.uncons t of
      Just (c,_) -> c
      Nothing -> 'x'
    isMyPairInThere pairc pair t = case T.uncons t of
      Just (_,left) -> (T.cons pairc left) `elem` (fdKeys pair)
      Nothing -> False
    as = filterTable (\t -> getHead t == 'a') revfd
    aes = filterTable (\t -> getHead t == 'ä') revfd
    asinaestoo = filterTable (isMyPairInThere 'ä' aes) as
    aesinastoo = filterTable (isMyPairInThere 'a' as) aes
    remerged = asinaestoo <> aesinastoo
  in
   mapTable T.reverse remerged

annotateWithPatterns :: FreqDist -> AnnotatedFreqDist
annotateWithPatterns fd =
  let patternAnnotMap :: [(Annotation, Token -> Bool)]
      patternAnnotMap = [
        (T.pack "1", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.j{-consonantal}.*"))),
        (T.pack "2", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.{-consonantal}.*"))),
        (T.pack "3", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal,+high}.[p,pp]{-consonantal}.*"))),
        (T.pack "4", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.[l,ll]{-consonantal}.*"))),
        (T.pack "5", fitsPattern (fromJust $ readPattern finnishInventory (T.pack "*{-consonantal}.[f,s,h,v]j{-consonantal}.*")))
        ]
  in
   annotateFD patternAnnotMap fd
                    
fitsPattern :: [Pattern] -> Token -> Bool
fitsPattern patt tkn =
  let
    phons = segment finnishInventory tkn
  in
   case phons of
     Just phs -> filterWord phs patt
     Nothing -> False
