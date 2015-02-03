module Hanalyze.Chisq where

import Data.List
import Statistics.Distribution.ChiSquared
import Statistics.Distribution

data ChiTest = ChiTest {
  chisq :: Double,
  p :: Double,
  sig :: Bool
  }


getMatrixSize :: [[Double]] -> (Int, Int)
getMatrixSize table =
  let
    rowl = length table
    colsl = nub $ map length table
  in
   case length colsl of
     1 -> (rowl,head colsl)
     _ -> error "different column length in matrix"

getChiSq :: [[Double]] -- ^The matrix
            -> Bool    -- ^Use Yates correction?
            -> Double  -- ^Chi squared
getChiSq table yates =
  let
    (rowl, coll) = getMatrixSize table
    rowsum = map sum table
    colsum = map sum $ transpose table
    grandsum = sum rowsum
    xrc = zip3 (concat table)  -- values
               (concat $ map (replicate rowl) rowsum) -- row sums for each value
               (concat $ replicate coll colsum) -- col sums for each value
    getChiSqForValue (val, row, col) =
      let expected = (row / grandsum) * (col / grandsum) * grandsum
          oe = abs (val - expected) - if yates then 0.5 else 0
      in
       oe * oe / expected
    chisq = sum $ map getChiSqForValue xrc
  in 
   chisq


runChiSqTest :: [[Double]] -- ^The matrix
             -> Bool       -- ^Use Yates correction?
             -> ChiTest
runChiSqTest table yates =
  let
    (rowl, coll) = getMatrixSize table
    savChisq = getChiSq table yates
    df = (rowl - 1) * (coll - 1)
    savP = cumulative (chiSquared df) savChisq
    savSig = case savP of
      x | x < 0.025 -> True
        | x > 0.975 -> True
      _ -> False
  in
   ChiTest savChisq savP savSig
