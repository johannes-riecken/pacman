-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PacmanForeign
  ( module Foreign.C
  , neighbors
  , filterCorners
  , v2ToTuple
  , startPerl
  , stopPerl
  ) where

import Data.List
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Linear
import System.IO.Unsafe

foreign import ccall
    neighborsImpl :: CInt -> CInt -> CInt -> CInt -> Ptr (V2 CInt)

{-# ANN neighbors "HLint: ignore Avoid restricted function" #-}
neighbors :: CInt -> CInt -> CInt -> CInt -> [(CInt,CInt)]
neighbors x y dir_x dir_y =
    fmap v2ToTuple .
        unsafePerformIO .
        peekArray 8 $ -- convert int[8] to haskell list
        neighborsImpl x y dir_x dir_y

foreign import ccall "filterCorners" filterCornersImpl :: CSize -> Ptr (V2 CInt) -> Ptr CSize -> IO (Ptr (V2 CInt))

{-# ANN filterCorners "HLint: ignore Avoid restricted function" #-}
filterCorners :: [V2 CInt] -> [V2 CInt]
filterCorners xs = unsafePerformIO $ do
  arr <- newArray xs
  p_outlen :: Ptr CSize <- malloc
  ret <- filterCornersImpl (genericLength xs) arr p_outlen
  outlen <- peek p_outlen
  peekArray (fromIntegral outlen) ret

-- foreign import ccall "walkLine" walkLineImpl :: Img -> CInt -> CInt -> Seen -> [Point]

foreign import ccall startPerl :: IO ()

foreign import ccall stopPerl :: IO ()

v2ToTuple :: V2 CInt -> (CInt,CInt)
v2ToTuple (V2 x y) = (x,y)

