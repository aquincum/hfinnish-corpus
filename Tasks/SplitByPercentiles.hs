module Tasks.SplitByPercentiles (task) where


import qualified Data.Vector.Unboxed as V
import           Hanalyze.FreqDist
import qualified Statistics.Quantile as Q
import           System.FilePath.Posix
import           Tasks.Task

task :: Task
task = Task
       doTask
       (Just FileName)
       "splitbypercentiles"
       "Split a frequency distribution into bins by percentiles. The -n parameter can specify how many bins to separate into; with 10 as default. Writes into files with suffixes _quant_X"


doTask :: [Flag] -> IO ()
doTask flags = do
  let minfn = getFlag flags FileName
  infn <- dieIfNoFN minfn
  let (dirname,fname) = splitFileName infn
  let mmaxn = getFlag flags MaxN
      maxn = case mmaxn of
        Nothing -> 10
        Just (MaxN (IntParam n)) -> n
  fd <- readFreqDist infn
  let freqs = V.fromList $ map (fromIntegral . snd) $ tToList fd
      quantiles = map (\n -> Q.continuousBy Q.s n maxn freqs) [0..maxn]
      fdquantiles = map (getFDBetweenFreqs fd quantiles) [1..maxn]
  mapM (\n -> let fn = dirname </> (fname ++ "_quant_" ++ show n)
              in
               saveTable (fdquantiles !! (n-1)) fn
       )
    [1..maxn]
  putStrLn "Splitting done!"

getFDBetweenFreqs :: FreqDist -> [Double] -> Int -> FreqDist
getFDBetweenFreqs fd quantiles n =
  let
    lower = quantiles !! (n-1)
    upper = (quantiles !! n) + if n == length quantiles then 0 else 1
    filterFunc freq = (fromIntegral freq >= lower) &&
                       (fromIntegral freq < upper)
  in
   filterByValTable filterFunc fd
