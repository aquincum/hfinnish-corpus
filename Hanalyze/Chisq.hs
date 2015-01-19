{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hanalyze.Chisq where

import Foreign
import Foreign.C.Types
import System.IO.Unsafe
import Unsafe.Coerce
import Data.List

foreign import ccall unsafe "test"
  c_test :: CInt -> CInt

foreign import ccall unsafe "getChiSq"
  c_getChiSq :: Ptr (Ptr CDouble) -- ^double **table
                -> CInt           -- ^int x
                -> CInt           -- ^int y
                -> CInt           -- ^int yates (bool)
                -> IO CDouble     -- ^result

foreign import ccall unsafe "chiSqTest"
  c_chiSqTest :: CInt             -- ^int x
              -> CInt             -- ^int y
              -> CDouble          -- ^double chiSq
              -> CDouble          -- ^@p@

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
  in unsafePerformIO $ do
          let ctable = (map . map) unsafeCoerce table
          rowptrs <- mapM newArray ctable
          fullptr <- newArray rowptrs
          let x = fromIntegral rowl
              y = fromIntegral coll
              yatesint = if yates
                         then (fromIntegral 1)
                         else (fromIntegral 0)
          res <- c_getChiSq fullptr x y yatesint
          return (unsafeCoerce res)
     
runChiSqTest :: [[Double]] -- ^The matrix
             -> Bool       -- ^Use Yates correction?
             -> ChiTest
runChiSqTest table yates =
  let (rowl, coll) = getMatrixSize table
      savChisq = getChiSq table yates
      savP = unsafeCoerce $ c_chiSqTest (fromIntegral rowl) (fromIntegral coll) (unsafeCoerce savChisq)
      savSig = case savP of
        x | x < 0.025 -> True
          | x > 0.975 -> True
        _ -> False
  in
    ChiTest savChisq savP savSig

mytest :: Int -> Int
mytest x = fromIntegral(c_test (fromIntegral x))

la = 2
