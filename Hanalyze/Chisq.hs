-- |This small module implements a chi square test, needed for some analyses
module Hanalyze.Chisq where

import Data.List
import Statistics.Distribution.ChiSquared
import Statistics.Distribution

-- |Data structure containing all information returned by the chi test.
data ChiTest = ChiTest {
  chisq :: Double, -- ^The chi square value itself
  p :: Double,  -- ^A p value
  sig :: Bool -- ^Significance
  }


-- |Returns the (row,column) size of a 2 dimension matrix. Quite unsafe as it throws an error
-- if the matrix is not square
getMatrixSize :: [[Double]] -> (Int, Int)
getMatrixSize table =
  let
    rowl = length table
    colsl = nub $ map length table
  in
   case length colsl of
     1 -> (rowl,head colsl)
     _ -> error "different column length in matrix"

-- |Calculates a chi square value on a __square__ matrix.
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

-- |Runs the chi square test on a __square__ matrix
runChiSqTest :: [[Double]] -- ^The matrix
             -> Bool       -- ^Use Yates correction?
             -> ChiTest    -- ^Result
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
