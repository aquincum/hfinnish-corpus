{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hanalyze.Chisq where

import Foreign
import Foreign.C.Types

foreign import ccall "test"
  c_test :: CInt -> CInt

mytest :: Int -> Int
mytest x = fromIntegral(c_test (fromIntegral x))

la = 2
